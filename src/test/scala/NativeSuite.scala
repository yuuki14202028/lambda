package com.yuuki14202028

import java.nio.file.{Files, Path}
import scala.sys.process.*

/** ソースを ARM64 のアセンブリにして動かし、出力と終了コードをインタプリタと突き合わせる（ARM64 の macOS だけ） */
class NativeSuite extends munit.FunSuite {

  private val prelude =
    """let addInt(x: i32)(y: i32): i32 = intrinsic[add_i32](x)(y)
      |let subInt(x: i32)(y: i32): i32 = intrinsic[sub_i32](x)(y)
      |let mulInt(x: i32)(y: i32): i32 = intrinsic[mul_i32](x)(y)
      |let divInt(x: i32)(y: i32): i32 = intrinsic[div_i32](x)(y)
      |let modInt(x: i32)(y: i32): i32 = intrinsic[mod_i32](x)(y)
      |let eqInt(x: i32)(y: i32): bool = intrinsic[eq_i32](x)(y)
      |let ltInt(x: i32)(y: i32): bool = intrinsic[lt_i32](x)(y)
      |let negInt(x: i32): i32 = intrinsic[neg_i32](x)
      |infixl 6 + = addInt
      |infixl 6 - = subInt
      |infixl 7 * = mulInt
      |infixl 7 / = divInt
      |infixl 7 % = modInt
      |infix 4 == = eqInt
      |infix 4 < = ltInt
      |prefix - = negInt
      |
      |type String = foreign.C.String
      |let print: String → i32 = foreign[String → i32] print_string_inline
      |let printInt: i32 → i32 = foreign[i32 → i32] print_int
      |let showInt: i32 → String = foreign[i32 → String] int_to_string
      |let showDouble: f64 → String = foreign[f64 → String] double_to_string
      |let concat: String → String → String = foreign[String → String → String] concat
      |let line(text: String): i32 = print(concat(text)("\n"))
      |let showBool(value: bool): String = if value then "true" else "false"
      |
      |data Option[a] = { Some(a), None }
      |data Pair[a][b] = { Pair(a)(b) }
      |rec data List[a] = { Nil, Cons(a)(List[a]) }
      |""".stripMargin

  private val isArm64Mac =
    System.getProperty("os.name").toLowerCase.contains("mac") && System.getProperty("os.arch") == "aarch64"

  private def compile(body: String): Lambda.Program =
    Compiler.compile(prelude + body) match {
      case Right(program) => program
      case Left(errors) => fail(Diagnostics.render(errors, "test.lam", prelude + body))
    }

  private def native(program: Lambda.Program): Interpreter.Result = {
    val directory = Files.createTempDirectory("lambda-native")
    val assembly = directory.resolve("out.s")
    val binary = directory.resolve("out")
    Files.writeString(assembly, Generator.generate(program))
    val linkerOutput = new StringBuilder
    val linked = Seq("clang", "-arch", "arm64", assembly.toString, "runtime/ffi.c", "-o", binary.toString).!(ProcessLogger(line => linkerOutput ++= line += '\n'))
    assertEquals(linked, 0, linkerOutput.toString)
    val output = new StringBuilder
    val exitCode = Process(binary.toString).#<(new java.io.ByteArrayInputStream(Array.emptyByteArray)).!(ProcessLogger(line => output ++= line += '\n'))
    Interpreter.Result(exitCode, output.toString)
  }

  // 終了コードは下位 8 ビットだけが残る。出力は行ごとに読むので、最後の改行の有無は比べない
  private def normalize(result: Interpreter.Result): Interpreter.Result =
    Interpreter.Result(result.exitCode & 0xff, result.output.stripSuffix("\n"))

  /** インタプリタと同じ結果になることを確かめる */
  private def check(name: String)(body: String)(using munit.Location): Unit = test(name) {
    assume(isArm64Mac, "ARM64 の macOS でだけ動かす")
    val program = compile(body)
    assertEquals(normalize(native(program)), normalize(Interpreter.run(program)))
  }

  check("main の結果が終了コードになる")("let main(): i32 = 42")

  check("終了コードは下位 8 ビット")("let main(): i32 = 300")

  check("i32 の演算: あふれると折り返す") {
    """let main(): i32 = {
      |  printInt(1 + 2 * 3 - 4);
      |  printInt(2147483647 + 1);
      |  printInt(-7 / 2);
      |  printInt(-7 % 3);
      |  printInt(100000 * 100000);
      |  printInt(-(-5));
      |  0
      |}
      |""".stripMargin
  }

  check("幅と符号の違う整数") {
    """let show8: i8 → String = foreign[i8 → String] long_to_string
      |let showU8: u8 → String = foreign[u8 → String] long_to_string
      |let show16: i16 → String = foreign[i16 → String] long_to_string
      |let showU16: u16 → String = foreign[u16 → String] long_to_string
      |let showU32: u32 → String = foreign[u32 → String] long_to_string
      |let show64: i64 → String = foreign[i64 → String] long_to_string
      |let showU64: u64 → String = foreign[u64 → String] long_to_string
      |let main(): i32 = {
      |  line(show8(intrinsic[add_i8](127i8)(1i8)));
      |  line(show8(intrinsic[neg_i8](100i8)));
      |  line(showU8(intrinsic[add_u8](200u8)(100u8)));
      |  line(showU8(intrinsic[sub_u8](1u8)(2u8)));
      |  line(show16(intrinsic[mul_i16](300i16)(300i16)));
      |  line(showU16(intrinsic[sub_u16](0u16)(1u16)));
      |  line(showU32(intrinsic[sub_u32](0u32)(1u32)));
      |  line(showU32(intrinsic[div_u32](4000000000u32)(3u32)));
      |  line(showU32(intrinsic[mod_u32](4000000000u32)(7u32)));
      |  line(show64(intrinsic[mul_i64](3000000000i64)(3000000000i64)));
      |  line(show64(intrinsic[sub_i64](0i64)(9000000000000000000i64)));
      |  line(show64(intrinsic[div_i64](intrinsic[neg_i64](9000000000000000000i64))(7i64)));
      |  line(showU64(intrinsic[div_u64](18446744073709551615u64)(2u64)));
      |  line(showU64(intrinsic[mod_u64](18446744073709551615u64)(10u64)));
      |  line(show64(intrinsic[xor_i64](255i64)(intrinsic[and_i64](15i64)(intrinsic[or_i64](1i64)(4i64)))));
      |  line(showBool(intrinsic[lt_u64](1u64)(18446744073709551615u64)));
      |  line(showBool(intrinsic[geq_u64](1u64)(18446744073709551615u64)));
      |  line(showBool(intrinsic[lt_i64](intrinsic[neg_i64](1i64))(1i64)));
      |  line(showBool(intrinsic[gt_u8](200u8)(100u8)));
      |  line(showBool(intrinsic[lt_i8](intrinsic[add_i8](127i8)(1i8))(0i8)));
      |  0
      |}
      |""".stripMargin
  }

  check("浮動小数点数") {
    """let main(): i32 = {
      |  line(showDouble(intrinsic[add_f64](1.25)(2.5)));
      |  line(showDouble(intrinsic[sub_f64](1.25)(2.5)));
      |  line(showDouble(intrinsic[mul_f64](1.5)(2.5)));
      |  line(showDouble(intrinsic[div_f64](1.0)(8.0)));
      |  line(showDouble(intrinsic[neg_f64](0.5)));
      |  line(showBool(intrinsic[lt_f64](1.5)(2.5)));
      |  line(showBool(intrinsic[geq_f64](1.5)(2.5)));
      |  line(showBool(intrinsic[eq_f64](1.5)(1.5)));
      |  line(showBool(intrinsic[neq_f64](1.5)(1.5)));
      |  line(showBool(intrinsic[gt_f32](intrinsic[mul_f32](1.5f32)(2.0f32))(2.5f32)));
      |  line(showBool(intrinsic[eq_f32](intrinsic[div_f32](intrinsic[neg_f32](1.0f32))(4.0f32))(intrinsic[sub_f32](0.25f32)(0.5f32))));
      |  line(showBool(intrinsic[leq_f32](0.5f32)(0.25f32)));
      |  0
      |}
      |""".stripMargin
  }

  check("比較と bool の演算") {
    """let main(): i32 = {
      |  line(showBool(1 < 2));
      |  line(showBool(2 < 1));
      |  line(showBool(-1 < 1));
      |  line(showBool(3 == 3));
      |  line(showBool(intrinsic[neq_i32](3)(3)));
      |  line(showBool(intrinsic[leq_i32](3)(3)));
      |  line(showBool(intrinsic[not_bool](true)));
      |  line(showBool(intrinsic[and_bool](true)(false)));
      |  line(showBool(intrinsic[or_bool](true)(false)));
      |  line(showBool(intrinsic[xor_bool](true)(true)));
      |  line(showBool(intrinsic[eq_bool](false)(false)));
      |  line(showBool(intrinsic[eq_char]('a')('b')));
      |  0
      |}
      |""".stripMargin
  }

  check("文字列と文字") {
    """let length: String → i32 = foreign[String → i32] string_length
      |let charAt: String → i32 → char = foreign[String → i32 → char] char_at
      |let substring: String → i32 → i32 → String = foreign[String → i32 → i32 → String] substring
      |let charCode: char → i32 = foreign[char → i32] char_code
      |let charFromCode: i32 → char = foreign[i32 → char] char_from_code
      |let charToString: char → String = foreign[char → String] char_to_string
      |let stringEqual: String → String → bool = foreign[String → String → bool] string_equal
      |let stringCompare: String → String → i32 = foreign[String → String → i32] string_compare
      |let stringToInt: String → i32 = foreign[String → i32] string_to_int
      |let readLine: unit → String = foreign[unit → String] read_line
      |let putChar: i32 → i32 = foreign[i32 → i32] put_char
      |let main(): i32 = {
      |  let greeting: String = concat("hello, ")("world") in {
      |    line(greeting);
      |    printInt(length(greeting));
      |    line(substring(greeting)(7)(3));
      |    printInt(charCode(charAt(greeting)(4)));
      |    line(charToString(charFromCode(charCode('a') + 1)));
      |    line(showBool(stringEqual(greeting)("hello, world")));
      |    line(showBool(stringEqual(greeting)("hello")));
      |    printInt(stringCompare("a")("b"));
      |    printInt(stringToInt("-123") + 1);
      |    printInt(length(readLine(())));
      |    line("tab\there \"quoted\" back\\slash");
      |    putChar(charCode('!'));
      |    putChar(10);
      |    length(greeting)
      |  }
      |}
      |""".stripMargin
  }

  check("クロージャは変数を捕まえる") {
    """let adder(x: i32): i32 → i32 = λy: i32. x + y
      |let compose(f: i32 → i32)(g: i32 → i32): i32 → i32 = λx: i32. f(g(x))
      |let twice(f: i32 → i32)(x: i32): i32 = f(f(x))
      |let main(): i32 = {
      |  let base: i32 = 100 in
      |  let scale: i32 = 3 in
      |  let f: i32 → i32 = compose(adder(base))(λn: i32. n * scale) in {
      |    printInt(f(5));
      |    printInt(twice(f)(1));
      |    printInt(twice(twice(adder(1)))(0));
      |    f(0)
      |  }
      |}
      |""".stripMargin
  }

  check("C の関数は引数を 1 つずつ受け取れる") {
    """let addInts: i32 → i32 → i32 = foreign[i32 → i32 → i32] add_ints
      |let main(): i32 = {
      |  let addTen: i32 → i32 = addInts(10) in
      |  let hello: String → String = concat("hello ") in {
      |    printInt(addTen(1) + addTen(2));
      |    line(hello("a"));
      |    line(hello("b"));
      |    0
      |  }
      |}
      |""".stripMargin
  }

  check("data と入れ子の match") {
    """rec let sum(xs: List[i32]): i32 = match xs {
      |  Nil => 0
      |  Cons(x)(rest) => x + sum(rest)
      |}
      |rec let map(f: i32 → i32)(xs: List[i32]): List[i32] = match xs {
      |  Nil => Nil[i32]
      |  Cons(x)(rest) => Cons(f(x))(map(f)(rest))
      |}
      |let firstTwo(xs: List[Option[i32]]): i32 = match xs {
      |  Cons(Some(a))(Cons(Some(b))(_)) => a + b
      |  Cons(Some(a))(_) => a
      |  _ => 0
      |}
      |let swap(p: Pair[i32][String]): Pair[String][i32] = match p { Pair(a)(b) => Pair(b)(a) }
      |let main(): i32 = {
      |  printInt(sum(map(λn: i32. n * n)(Cons(1)(Cons(2)(Cons(3)(Nil[i32]))))));
      |  printInt(firstTwo(Cons(Some(4))(Cons(Some(5))(Nil[Option[i32]]))));
      |  printInt(firstTwo(Cons(Some(4))(Cons(None[i32])(Nil[Option[i32]]))));
      |  printInt(firstTwo(Cons(None[i32])(Nil[Option[i32]])));
      |  match swap(Pair(7)("seven")) { Pair(name)(n) => { line(name); n } }
      |}
      |""".stripMargin
  }

  check("rec: 再帰・相互再帰・式の中の rec") {
    """rec let factorial(n: i32): i32 = if n == 0 then 1 else n * factorial(n - 1)
      |rec let fib(n: i32): i32 = if n < 2 then n else fib(n - 1) + fib(n - 2)
      |rec {
      |  let isEven(n: i32): bool = if n == 0 then true else isOdd(n - 1)
      |  let isOdd(n: i32): bool = if n == 0 then false else isEven(n - 1)
      |}
      |let main(): i32 = {
      |  printInt(factorial(10));
      |  printInt(fib(20));
      |  line(showBool(isEven(10)));
      |  line(showBool(isOdd(10)));
      |  let offset: i32 = 1000 in
      |  rec let down(n: i32): i32 = if n == 0 then offset else 1 + down(n - 1) in
      |  rec {
      |    let ping(n: i32): i32 = if n == 0 then offset else pong(n - 1)
      |    let pong(n: i32): i32 = if n == 0 then -offset else ping(n - 1)
      |  } in {
      |    printInt(down(5));
      |    printInt(ping(7));
      |    ping(4) / 10
      |  }
      |}
      |""".stripMargin
  }

  check("深い再帰（末尾でない呼び出しが 100000 段）") {
    """rec let depth(n: i32): i32 = if n == 0 then 0 else 1 + depth(n - 1)
      |rec let range(n: i32): List[i32] = if n == 0 then Nil[i32] else Cons(n)(range(n - 1))
      |rec let sum(xs: List[i32]): i32 = match xs {
      |  Nil => 0
      |  Cons(x)(rest) => x + sum(rest)
      |}
      |let main(): i32 = {
      |  printInt(depth(100000));
      |  printInt(sum(range(100000)));
      |  0
      |}
      |""".stripMargin
  }

  check("ブロックは順に評価し、&& と || は右を必要なときだけ評価する") {
    """let noisy(name: String)(value: bool): bool = { line(name); value }
      |rec let loop(n: i32): bool = loop(n)
      |let main(): i32 = {
      |  line(showBool(noisy("a")(false) && noisy("b")(true)));
      |  line(showBool(noisy("c")(true) && noisy("d")(false)));
      |  line(showBool(noisy("e")(true) || noisy("f")(false)));
      |  line(showBool(noisy("g")(false) || noisy("h")(true)));
      |  if (1 == 2 && loop(0)) || 1 == 1 then 7 else 8
      |}
      |""".stripMargin
  }

  // インタプリタは末尾呼び出しでも Scala のスタックを使うので、この回数は動かせない。結果は直接確かめる。
  // down の回数は、フレームを畳んでいなければ確保したスタック（1GB）に収まらない数
  test("末尾呼び出しはスタックを使わない（10000000 回）") {
    assume(isArm64Mac, "ARM64 の macOS でだけ動かす")
    val program = compile(
      """rec let count(n: i32)(acc: i32): i32 = if n == 0 then acc else count(n - 1)(acc + 1)
        |rec {
        |  let isEven(n: i32): bool = if n == 0 then true else isOdd(n - 1)
        |  let isOdd(n: i32): bool = if n == 0 then false else { let m: i32 = n - 1 in isEven(m) }
        |}
        |let main(): i32 = {
        |  printInt(count(10000000)(0));
        |  line(showBool(isEven(10000000)));
        |  rec let down(n: i32): i32 = if n == 0 then 3 else down(n - 1) in down(50000000)
        |}
        |""".stripMargin)
    assertEquals(native(program), Interpreter.Result(3, "10000000\ntrue\n"))
  }
}
