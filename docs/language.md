# 言語の説明

この文書は、言語を上から順に説明します。`lam` と書いたコード例はすべて、先頭に `import "std/prelude"` を付ければそのままコンパイルできます（テストで確かめています）。

## 1. 方針

- **型はすべて書く**。型推論はありません。束縛には型注釈を書き、多相関数に渡す型も `f[T]` と書きます。
- **System Fω の範囲に収める**。値のラムダ、型の抽象と適用、型レベルの関数とカインドがあります。それより強い型の仕組み（依存型、GADT など）は入れていません。
- **暗黙に起きることを減らす**。再帰は `rec` と書いた所だけ、宣言は書いた順にだけ見えます。暗黙なのは trait のインスタンスの解決だけで、それも明示的に上書きできます。

プログラムは宣言の並びで、`main` から始まります。`main` の結果が終了コードです。

```lam
let main(): i32 = {
  println("hello");
  0
}
```

```bash
sbt "run hello.lam build/out.s"
clang -arch arm64 build/out.s runtime/ffi.c -o build/out && ./build/out
```

## 2. 値と関数

### 束縛

`let 名前: 型 = 式` で値に名前を付けます。式の中では `let … in 式` と書きます。

```lam
let answer: i32 = 42

let doubled: i32 =
  let half: i32 = 21 in half + half
```

### 関数

関数は `λ引数: 型. 本体` で、型は `引数の型 → 結果の型` です。引数は 1 つずつ取り、`f(x)(y)` と 1 つずつ渡します。

```lam
let add: i32 → i32 → i32 = λx: i32. λy: i32. x + y

let three: i32 = add(1)(2)
```

名前のある関数は、引数を名前の後ろに書けます。意味は上の書き方と同じです。

```lam
let add(x: i32)(y: i32): i32 = x + y

// 引数のない関数は unit を取る
let now(): i32 = 0
let later: i32 = now()

// 2 引数の関数は、引数のあいだに書いて呼べる
let five: i32 = 2 add 3
```

### 基本の型と式

| 型 | 例 |
| --- | --- |
| `i8` `i16` `i32` `i64` `isize` `u8` `u16` `u32` `u64` `usize` | `42`、`42i64`、`7usize` |
| `f32` `f64` | `1.5`、`1.5f32` |
| `bool` | `true`、`false` |
| `char` | `'a'`、`'0'`、`' '`、`'\n'` |
| `foreign.C.String`（別名 `String`） | `"text"` |
| `unit` | `()` |

```lam
let pick(b: bool): i32 = if b then 1 else 2

// ブロック: 途中の式の値は捨て、最後の式が値になる
let noisy(): i32 = {
  println("first");
  println("second");
  3
}

// 文字列埋め込み: { } の中の式を show で文字列にして埋める
let message(n: i32): String = `n = {n}, doubled = {n + n}`
```

演算子（`+ - * / %`、`== != < <= > >=`、`&& ||`、`!`、ビット演算 `& | ^`）は、左の値の型から決まる関数の呼び出しです。`a + b` は、`a` が `i32` なら関数 `__add_i32` を呼びます。組み込みの型の分は `std/operators` にあり、自分の型にも定義できます（型の名前の `.` は `_` にします）。

```lam
data Money = { MkMoney(i32) }

let __add_Money(x: Money)(y: Money): Money = match x {
  MkMoney(a) => match y {
    MkMoney(b) => MkMoney(a + b)
  }
}

let total: Money = MkMoney(100) + MkMoney(20)
```

### コメント

```lam
// 行の終わりまで
let x: i32 = 1 /* 式の途中にも書ける /* 入れ子にもできる */ */ + 2
```

## 3. 多相

### 型の抽象と適用

型を引数に取る値は `Λa. 式` で作り、型は `∀a. 型` です。使うときは `f[型]` と型を渡します。型は推論されないので、省略できません。

```lam
let identity: ∀a. a → a = Λa. λx: a. x

let one: i32 = identity[i32](1)

// 名前のある関数では、型の引数を [ ] で書ける。上の identity と同じ意味
let same[a](x: a): a = x

let composed[a][b][c](f: b → c)(g: a → b): a → c = λx: a. f(g(x))
```

### カインドと型の関数

型にも種類（カインド）があります。値の型は `*`、型を 1 つ取る型構成子（`Option` や `List`）は `* → *` です。型の引数のカインドは `[f: * → *]` と書き、省略すると `*` です。

```lam
// f は型構成子。Option を渡せば Option[Option[i32]] を作る関数になる
let wrapTwice[f: * → *](wrap: ∀a. a → f[a])(x: i32): f[f[i32]] = wrap[f[i32]](wrap[i32](x))

let nested: Option[Option[i32]] = wrapTwice[Option](Λa. λx: a. Some[a](x))(1)
```

型の別名は `type` で定義します。引数を取る別名や、型構成子の別名も書けます。型の中で関数を直接書くときは `λa. 型` です。

```lam
type Name = String
type Transform[a] = a → a
type Failing = Either[String]

let rename: Transform[Name] = λn: Name. n

let failed: Failing[i32] = Left[String][i32]("no value")

// 型レベルの関数をその場で書く: 「a と a の組」という型構成子
let diagonal: (λa. Pair[a][a])[i32] = Pair[i32][i32](1)(1)
```

## 4. データ型とパターン

### data

`data` は、構成子を並べてデータ型を定義します。構成子は関数で、引数を 1 つずつ取ります。

```lam
data Shape = { Circle(i32), Rectangle(i32)(i32), Dot }

data Box[a] = { MkBox(a) }

let unit: Shape = Rectangle(1)(1)
let boxed: Box[i32] = MkBox[i32](42)
```

### match

`match` は値を構成子で場合分けします。すべての値がどれかの分岐に当たらなければコンパイルエラーで、当たらない値の例が示されます。

```lam
data Shape = { Circle(i32), Rectangle(i32)(i32), Dot }

let area(s: Shape): i32 = match s {
  Circle(r) => 3 * r * r
  Rectangle(w)(h) => w * h
  Dot => 0
}
```

パターンには次のものが書けます。

| パターン | 意味 |
| --- | --- |
| `C(p)(q)` | 構成子。引数にもパターンを書ける（入れ子） |
| `x` | 変数。どの値にも当たり、その値を束縛する |
| `_` | どの値にも当たり、束縛しない |
| `0`、`-1`、`'a'`、`true`、`"text"` | リテラル |
| `p \| q` | どちらかに当たる。どの選択肢も同じ変数を同じ型で束縛する |
| `p if 条件` | パターンに当たり、条件が真のとき。偽なら後続の分岐を試す |

引数のない名前は、その名前の構成子がスコープにあれば構成子、なければ変数です。

```lam
let describe(xs: List[Option[i32]]): String = match xs {
  Nil => "empty"
  Cons(Some(0))(_) => "starts with zero"
  Cons(Some(n))(Nil) if n < 0 => "one negative number"
  Cons(Some(_))(Nil) | Cons(None)(Nil) => "one element"
  _ => "several elements"
}
```

### 束縛でのパターン

`let` でも値を分解できます。分岐が 1 つの `match` と同じで、すべての値に当たるパターンに限ります（引数のある構成子か `_`）。注釈は、分解する値全体の型です。

```lam
let swap(p: Pair[i32][bool]): Pair[bool][i32] =
  let Pair(n)(flag): Pair[i32][bool] = p in Pair[bool][i32](flag)(n)
```

## 5. 再帰

再帰は `rec` を付けた宣言だけができます。`rec` のない宣言は、自分自身も、後ろに書かれた宣言も見えません。

```lam
rec let factorial(n: i32): i32 = if n == 0 then 1 else n * factorial(n - 1)

rec data Tree = { Leaf, Branch(Tree)(i32)(Tree) }

rec let size(t: Tree): i32 = match t {
  Leaf => 0
  Branch(left)(_)(right) => size(left) + 1 + size(right)
}
```

互いを参照する宣言は、同じ種類のものを `rec { … }` で囲みます。関数、データ型、インスタンス（6 章）に使えます。

```lam
rec {
  let isEven(n: i32): bool = if n == 0 then true else isOdd(n - 1)
  let isOdd(n: i32): bool = if n == 0 then false else isEven(n - 1)
}

rec {
  data Folder = { MkFolder(String)(Entries) }
  data Entries = { NoEntries, MoreEntries(Folder)(Entries) }
}
```

式の中でも同じに書けます。

```lam
let countdown(n: i32): i32 =
  rec let loop(k: i32)(acc: i32): i32 = if k == 0 then acc else loop(k - 1)(acc + k) in
  loop(n)(0)
```

決まりが 2 つあります。

- **`rec` を使った所の停止性は保証されません。** 止まらない定義も書けます。
- **`rec` の中で、関数でない値は組の誰も参照できません。** `rec let x: i32 = x + 1` は、`x` を作っている途中で `x` を読むことになるので、コンパイルエラーです。

関数の本体の最後にある呼び出しは、スタックを消費しません。上の `loop` や `isEven` / `isOdd` は、何回くり返しても深くなりません。

### fold

`fold` は、再帰的なデータ型を畳み込む簡単な書き方です。自分自身の型のフィールドには、すでに畳み込んだ結果が束縛されます。

```lam
let sum(xs: List[i32]): i32 = fold xs as i32 {
  Nil => 0
  Cons(x)(rest) => x + rest
}
```

## 6. trait とインスタンス

### trait と impl

`trait` は、型に対する操作の一覧です。`impl` は、ある型についてその操作を定義します。メソッドを呼ぶときは、どの型についてかを `[型]` で書きます。

```lam
trait Describe[a] {
  def describe(x: a): String
}

impl Describe[bool] {
  def describe(x: bool): String = if x then "yes" else "no"
}

let answer: String = describe[bool](true)
```

`impl` のメソッドは、引数を並べる書き方（`def m(x: T): U = …`）でも、値を書く書き方（`def m = λx: T. …`）でも書けます。

### 制約

「この型には `Describe` のインスタンスがある」ことを要求するには、宣言の末尾に `with` を書きます。関数の中では、その制約をインスタンスとして使えます。

```lam
trait Describe[a] {
  def describe(x: a): String
}

let twice[a](x: a): String with Describe[a] = `{describe[a](x)} {describe[a](x)}`
```

`impl` にも `with` を書けます。「`a` にインスタンスがあれば、`Option[a]` にもある」という意味になります。

```lam
trait Describe[a] {
  def describe(x: a): String
}

impl Describe[i32] {
  def describe(x: i32): String = show[i32](x)
}

impl[a] Describe[Option[a]] with Describe[a] {
  def describe(x: Option[a]): String = match x {
    Some(v) => `some {describe[a](v)}`
    None => "nothing"
  }
}

let text: String = describe[Option[Option[i32]]](Some[Option[i32]](Some[i32](1)))
```

インスタンスは宣言と同じく、書いた後でだけ見えます。メソッドの本体から自分自身のインスタンスを使うときは `rec impl` と書きます（再帰的なデータ型のインスタンスは、たいていこれです）。互いを必要とするインスタンスは `rec { impl …  impl … }` で囲みます。

```lam
trait Describe[a] {
  def describe(x: a): String
}

rec impl[a] Describe[List[a]] with Describe[a] {
  def describe(xs: List[a]): String = match xs {
    Nil => "end"
    Cons(x)(rest) => `{describe[a](x)}, {describe[List[a]](rest)}`
  }
}
```

### スーパークラスとデフォルト

trait の `with` は、スーパークラスです。インスタンスを作るには、スーパークラスのインスタンスが先に要ります。逆に、その trait の制約があれば、スーパークラスのメソッドも使えます。

メソッドに本体を書くと、それがデフォルトになります。`impl` で省略すると、デフォルトが使われます。デフォルトの本体からは、デフォルトを持たないメソッドが普通の関数として見えます（型の引数を付けずに呼びます）。別のデフォルトや自分自身は見えないので、デフォルトで再帰は起きません。

```lam
trait Equal[a] {
  def same(x: a)(y: a): bool
  def differ(x: a)(y: a): bool = !same(x)(y)
}

trait Ordered[a] with Equal[a] {
  def before(x: a)(y: a): bool
  // スーパークラスのメソッドは、普通に型の引数を付けて呼ぶ
  def notAfter(x: a)(y: a): bool = before(x)(y) || same[a](x)(y)
}

impl Equal[i32] {
  def same(x: i32)(y: i32): bool = x == y
}

impl Ordered[i32] {
  def before(x: i32)(y: i32): bool = x < y
}

let checks: bool = differ[i32](1)(2) && notAfter[i32](2)(2)
```

### 型構成子の trait と derive

trait の引数は型構成子でもよく、標準ライブラリの `Functor` / `Foldable` / `Traversable` / `Applicative` / `Monad` はそうなっています。`Functor` / `Foldable` / `Traversable` のインスタンスは、データ型の形から `derive` で作れます。

```lam
rec data Rose[a] = { Node(a)(List[Rose[a]]) }

derive Functor[Rose]
derive Foldable[Rose]

let tree: Rose[i32] = Node[i32](1)(Cons[Rose[i32]](Node[i32](2)(Nil[Rose[i32]]))(Nil[Rose[i32]]))

let total: i32 = foldLeft[Rose][i32][i32](λacc: i32. λx: i32. acc + x)(0)(map[Rose][i32][i32](λx: i32. x * 10)(tree))
```

### 量化された制約

制約そのものを多相にできます。`∀a. Show[a] ⇒ Show[f[a]]` は、「どの `a` についても、`Show[a]` があれば `Show[f[a]]` がある」という要求です。型構成子を引数に取る型のインスタンスに使います。

```lam
// f を何回も重ねた型
rec data Fix[f: * → *] = { In(f[Fix[f]]) }

rec impl[f: * → *] Show[Fix[f]] with ∀a. Show[a] ⇒ Show[f[a]] {
  def show(x: Fix[f]): String = match x {
    In(inner) => show[f[Fix[f]]](inner)
  }
}
```

束縛する変数が型構成子のときは、カインドを書きます（`∀(m: * → *). Monad[m] ⇒ Monad[t[m]]`）。量化された制約は、スーパークラスにも、別の量化された制約の前提（括弧で囲む）にも書けます。

## 7. インスタンスを明示的に扱う

インスタンスの実体は、メソッドを並べた値（辞書）です。trait `C` は辞書の型でもあり、`C[i32]` は「`i32` についての `C` の辞書」の型です。普段は解決に任せますが、辞書を値として作り、渡すこともできます。

| 書き方 | 意味 |
| --- | --- |
| `summon[C[T]]` | 解決されたインスタンスの辞書 |
| `impl C[T] { def m = … }`（式） | 辞書をその場で作る。インスタンスとしては登録されない |
| `f[T](with d)` | 解決の代わりに、辞書 `d` を渡す |
| `using d in e` | `e` の中では、`d` をインスタンスとして解決に使う |

同じ型に、場所によって違う実装を使えます。

```lam
trait Ranked[a] {
  def before(x: a)(y: a): bool
}

impl Ranked[i32] {
  def before(x: i32)(y: i32): bool = x < y
}

let first[a](x: a)(y: a): a with Ranked[a] = if before[a](x)(y) then x else y

// 逆の順序の辞書を、与えられた辞書から作る
let reversed[a](order: Ranked[a]): Ranked[a] = impl Ranked[a] {
  def before(x: a)(y: a): bool = before[a](with order)(y)(x)
}

let smallest: i32 = first[i32](1)(2)
let largest: i32 = first[i32](with reversed[i32](summon[Ranked[i32]]))(1)(2)
let alsoLargest: i32 = using reversed[i32](summon[Ranked[i32]]) in first[i32](1)(first[i32](2)(3))
```

式としての `impl` は、スーパークラスの辞書をその位置で解決します。インスタンスがなければ、`using` で先に辞書をスコープに入れます。

## 8. context 記法

`context[M] { … }` は、モナド `M` の計算を順に並べる書き方です。`名前: 型 = 式;` は、`M[型]` の計算の結果を名前に束縛します。最後の式が全体の結果で、`pure` で包まれます。`flatMap` と `pure` の呼び出しに書き換えられるので、`M` には `Monad` のインスタンスが要ります。

```lam
let safeDivide(x: i32)(y: i32): Option[i32] = if y == 0 then None[i32] else Some[i32](x / y)

// どこかが None なら、全体が None
let compute(a: i32)(b: i32)(c: i32): Option[i32] = context[Option] {
  first: i32 = safeDivide(a)(b);
  second: i32 = safeDivide(first)(c);
  let bonus: i32 = 1;
  first + second + bonus
}
```

途中に `let`（モナドでない普通の束縛）と `rec let` も書けます。束縛にはパターンも書けます。

```lam
let both(p: Option[Pair[i32][i32]]): Option[i32] = context[Option] {
  Pair(x)(y): Pair[i32][i32] = p;
  x + y
}
```

## 9. module

### 名前空間

`module 名前 { … }` は、宣言をまとめて名前空間を作ります。外からは `名前.メンバー` と書き、`open` すると名前だけで書けます。module は入れ子にできます。

```lam
module Geometry {
  data Point = { MkPoint(i32)(i32) }

  let origin: Point = MkPoint(0)(0)

  let moveRight(p: Point)(dx: i32): Point = match p {
    MkPoint(x)(y) => MkPoint(x + dx)(y)
  }
}

let start: Geometry.Point = Geometry.moveRight(Geometry.origin)(3)

open Geometry

let further: Point = moveRight(start)(2)
```

### 公開する範囲

- `private` を付けた宣言は、module の外から見えません。
- `opaque type T = …` は、module の中では右辺と同じ型、外では中身のわからない別の型です。
- `opaque data` は、型の名前だけを公開し、構成子を隠します。

外からは、module が公開する関数を通してだけ値を作り、読めます。

```lam
module Counter {
  opaque type T = i32

  private let step: i32 = 1

  let zero: T = 0
  let next(c: T): T = c + step
  let read(c: T): i32 = c
}

let two: Counter.T = Counter.next(Counter.next(Counter.zero))
let value: i32 = Counter.read(two)
```

インスタンス（`impl` / `derive`）は、module の中に書いてもプログラム全体で有効です。

### import

`import "パス"` は、そのファイルの宣言をその場に取り込みます。`std/…` は同梱の `stdlib/` を、それ以外は取り込む側のファイルからの相対パスを読みます。top-level では、同じファイルは一度だけ取り込まれます。module の中に書くと、その module の中に取り込みます。

## 10. C の関数を呼ぶ

`foreign[型] 名前` は、その名前の C の関数を、指定した型の値として参照します。実装は `runtime/ffi.c` にあり、リンクするときに一緒に渡します。

```lam
let printNumber: i32 → i32 = foreign[i32 → i32] print_int
```

## 11. 標準ライブラリ

`import "std/prelude"` で全部が入ります。個別に `import "std/list"` のようにも取り込めます。

| ファイル | 内容 |
| --- | --- |
| `operators` | 組み込みの型の演算子 |
| `option` `either` `pair` `list` `id` | `Option`（`Some` / `None`）、`Either`（`Left` / `Right`）、`Pair`、`List`（`Nil` / `Cons`）、`Id` と、リストの関数 |
| `eq` `ord` | `Eq`（`eq` / `neq`）、`Ord`（`lt` / `leq` / `gt` / `geq`）。基本の型と、`Option` / `Either` / `Pair` / `List`（辞書順）のインスタンス |
| `semigroup` `monoid` | `append`、`empty` |
| `functor` `foldable` `traversable` `applicative` `monad` | `map`、`foldRight` / `foldLeft`、`traverse`、`pure` / `apply`、`flatMap` |
| `char` | module `Char`: `isDigit`、`isLetter`、`isSpace`、`toUpper`、`code` など |
| `string` | 型 `String` と module `Str`: `length`、`charAt`、`slice`、`toList`、`toInt` など。文字列の `==`・`<`・`+` |
| `show` | `Show`（`show`）と、基本の型・コンテナのインスタンス |
| `io` | `print`、`println`、`display`、`readLine` |
| `function` | `id`、`const`、`compose`、`flip`、`curry`、`uncurry` |

## 12. エラーの読み方

エラーは、原因になった部分に下線を引いて示します。`-` の下線は、関係する別の位置です。`note` は起きたことの説明、`help` は直し方です。

```
error: mismatched types
 --> main.lam:3:28
  |
3 | let double(n: i32): bool = n + n
  |                            ^^^^^ expected `bool`, found `i32`
  |                     ---- expected `bool` because of this type annotation
```

互いに関係のない宣言のエラーは、まとめて報告されます。ある宣言が失敗すると、それを使っている宣言のエラーは（連鎖なので）報告されません。

よくある原因には、直し方が示されます。

| 起きたこと | 示されるもの |
| --- | --- |
| 名前を打ち間違えた | 似た名前 |
| 後ろに書いた宣言を使った | 宣言を前に移すか、`rec { }` で囲む |
| `rec` を付け忘れた | `rec let` / `rec data` / `rec impl` と書く |
| インスタンスがない | 宣言済みのインスタンスの一覧、どのインスタンスの文脈で必要になったか、`with` 節を足す案内 |
| `private` な名前を外から使った | その名前が `private` であること |
| `opaque type` を中身の型として使った | module の外では別の型であること |
| 演算子が定義されていない | `std/operators` を取り込むか、その型の演算子の関数を定義する |
| `match` が値を取りこぼした | 当たらない値の例 |
| 古い書き方（`let rec`、`fold … with`）、`->` | 今の書き方 |

サンプルは `examples/` に、エラーの表示の例は `src/test/diagnostics/`（`.lam` と、表示されるエラーの `.txt`）にあります。
