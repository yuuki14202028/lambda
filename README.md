# Lambda

Scala 3 で実装した、小さな型付きラムダ計算系言語（System Fω 相当 + trait）のコンパイラです。
`.lam` ソースを型検査し、ARM64/macOS 向けアセンブリを生成して clang でリンクします。

## 特徴

- **System Fω**: 値のラムダ `λx: T. e`、型抽象 `ΛA. e`、型適用 `f[T]`、型レベルラムダとカインド（`*`, `* → *`）。型注釈と型適用はすべて明示で、HM 型推論は行いません
- **再帰**: 再帰は `rec` を付けた宣言に限ります。`rec let f(n: i32): i32 = …`、`rec data List[a] = …`、`rec impl[a] Show[List[a]] with Show[a] { … }`。相互再帰は、同じ種類の宣言を `rec { … }` で囲みます（`rec { let isEven …  let isOdd … }`、`rec { data Tree …  data Forest … }`、`rec { impl …  impl … }`）。`derive` は `impl` と同じ組に入れられ、相互再帰のデータ型には `rec { derive Functor[Tree]  derive Functor[Forest] }` と書きます。式の中（`rec let … in e`、`rec { … } in e`）と context ブロックの中（`rec { … };`）でも同じに書けます。`rec` を使った所の停止性は保証しません
- **代数的データ型**: `data` / 再帰型は `rec data`。`match e { パターン => … }` のパターンには、構成子（入れ子にできる。`Cons(x)(Cons(y)(_))`）・変数・`_`・リテラル（数・文字・bool・文字列）・選択肢（`0 | 1`）が書け、分岐には条件を付けられます（`Some(n) if n > 0 => …`）。すべての値がどれかの分岐に当たること（網羅性）が必須で、当たらない値があればその例を示し、到達不能な分岐もエラーにします。`fold e as T { C(x)(acc) => … }` で再帰型を畳み込みます（再帰位置の束縛子は畳み込み済みの結果）。バックエンドでは Church encoding に変換されます
- **trait**: `trait` / `impl` による型クラス。高階カインドの trait（`Monad[m: * → *]` など）、`with C[a]` 節（宣言末尾）による制約・スーパークラスに対応。メソッド自身の型変数にも制約を付けられます（`def traverse[g: * → *][a][b](f: a → g[b])(xs: t[a]): g[t[b]] with Applicative[g]`。impl 側のメソッドにも同じ `with` 節を書きます）。trait のメソッドにはデフォルトを書けます（`def neq(x: a)(y: a): bool = !eq(x)(y)`。impl で省略するとデフォルトが使われます）。デフォルトの本体からは、デフォルトを持たないメソッドが普通の関数として見え（型適用なしで `eq(x)(y)`）、スーパークラスの制約が使えます。別のデフォルトや自身のインスタンスは見えないので、辞書は自分自身を参照せず、再帰にはなりません。制約には量化された制約も書けます（`with ∀a. Show[a] ⇒ Show[f[a]]`。「どの `a` についても、`Show[a]` があれば `Show[f[a]]` がある」ことを要求し、本体の中ではインスタンスのように使えます。`Fix[f]` のような高階の不動点型のインスタンスに使います。前提にも量化された制約を書け（括弧で囲む）、スーパークラスにも書けます: `trait MonadTrans[t: (* → *) → * → *] with ∀(m: * → *). Monad[m] ⇒ Monad[t[m]]`）。インスタンスは明示的にも扱えます: `summon[Show[i32]]` は解決されたインスタンスの辞書（型は `Show[i32]`）、`f[i32](with d)` は解決の代わりに辞書の値 `d` を渡します（同じ型に対する別の実装を、その場で選べます）、`using d in e` は `e` の中で辞書 `d` をインスタンスとして解決に使います。型適用が明示なので辞書解決は決定的で、型検査の中で行われ、辞書渡しコードへ脱糖されます。インスタンスは値や関数と同じく、宣言してから使います。メソッド本体から自身のインスタンスを使う再帰的なインスタンスは `rec impl[a] Show[List[a]] with Show[a] { ... }` と明示します
- **derive**: `derive Functor[Option]` / `derive Foldable[List]` / `derive Traversable[List]` で、データ型の構造から `map` / `foldRight` / `foldLeft` / `traverse` のインスタンスを自動導出します。導出対象は構造から一意に決まる trait（正準シグネチャと α 同値なもの）に限り、負の位置の出現や畳み込めないフィールドはコンパイルエラーになります
- **context 記法**: do 記法相当。`context[M] { x: T = e; ...; result }` を `Monad` trait の `pure` / `flatMap` 呼び出しへ脱糖します
- **糖衣構文**: 関数定義 `let f[A](x: T)(y: U): R = e`、ブロック式 `{ e; ...; result }`、文字列埋め込み `` `result = {compute()}\n` ``、2 引数関数の中置適用 `120 safeDiv 5`
- **コメント**: `// …`（行末まで）と `/* … */`（入れ子にできる）
- **import**: `import "std/operators"` は同梱の `stdlib/` を、それ以外のパスはソースファイルからの相対で解決し、ファイルの宣言をその場に展開します（module の中に書けば、その module の中に展開します）
- **module**: `module M { 宣言… }` で名前空間を作ります（入れ子可）。外からは `M.x` / `M.T` と修飾して参照し、`open M` で修飾なしでも書けるようになります。`private` を付けた宣言は外から見えません。`opaque type T = i32` は module の中では `i32` と同じ型、外では中身のわからない型になり、`opaque data` は型の名前だけを公開して構成子を隠します。module の中からは、自身や外側の module を修飾子にした名前（`M` の中の `M.x`）も書けます。インスタンス（`impl` / `derive`）は module の中に書いてもプログラム全体で有効です。`opaque type` に対する `impl` は、書いた場所での型に対するインスタンスになります（module の中では表現の型、外では抽象型）
- **C FFI**: `foreign[T → U] name` で C 関数を参照。実装は `runtime/ffi.c`
- 関数値はコードポインタ + 環境ポインタのクロージャとしてコンパイルされます。末尾位置の呼び出しはジャンプになるので、末尾再帰でループを書けます（メモリは確保するだけで解放しません）

組み込み型は次のとおりです。

```text
i8 i16 i32 i64 isize  u8 u16 u32 u64 usize  f32 f64
char bool unit
foreign.C.String  foreign.C.VoidPtr  foreign.C.Ptr[T]
```

`+` や `==` などの演算子は型ごとに `__add_i32` のような関数へ解決されます。
これらは `stdlib/operators.lam` に `intrinsic` として定義されているため、演算子を使うには `import "std/operators"` が必要です。

## 使い方

必要なもの: ARM64 macOS、sbt、clang。

```bash
./run.sh          # main.lam をコンパイル → clang でリンク → 実行（終了コードを表示）

sbt "run <src.lam> <out.s>"                              # 任意のソースをアセンブリへ
clang -arch arm64 build/out.s runtime/ffi.c -o build/out # 手動リンク

sbt test                        # 全テスト（munit）。実際にコンパイル・実行する end-to-end テストを含みます
./benchmarks/run_fibonacci.sh   # 他言語とのベンチマーク比較
```

## エラー表示

コンパイルエラーは、原因になった部分式に下線を引き、関係する位置（期待される型を決めた注釈など）と直し方を添えて表示します。

```
error: mismatched types
 --> main.lam:3:28
  |
3 | let double(n: i32): bool = n + n
  |                            ^^^^^ expected `bool`, found `i32`
  |                     ---- expected `bool` because of this type annotation
```

```
error: no instance for `Show[bool]`
 --> main.lam:4:19
  |
4 | let main(): i32 = show[Box[Box[bool]]](MkBox[Box[bool]](MkBox[bool](true)))
  |                   ^^^^^^^^^^^^^^^^^^^^ this needs the instance
  |
  = note: required for `Show[Box[bool]]` by `impl[a] Show[Box[a]] with Show[a]`
  = note: required for `Show[Box[Box[bool]]]` by `impl[a] Show[Box[a]] with Show[a]`
  = note: instances declared before this point:
            impl[a] Show[Box[a]] with Show[a]
```

似た名前の候補、`rec` の付け忘れ、後で宣言されている名前、`private` な名前、`opaque type` と表現の取り違え、足りない `with` 節、古い構文（`let rec` など）といった、よくある原因には直し方を示します。import したファイルの中のエラーは、そのファイルの位置で表示します。表示の例は `src/test/diagnostics/`（`.lam` と、表示されるエラーの `.txt`）にあります。

## アーキテクチャ

```
ParserAST (cats-parse)
  → ImportResolver   import の解決・展開
  → TAnalyser        型検査（trait / context / derive / module は構文を保ったまま型付け。制約と名前の解決もここで行う）
  → ModuleDesugar    module を平らな宣言列に戻し、opaque type を表現へ読み替える
  → PatternDesugar   match のパターンを、構成子 1 段の分岐と if / let へ展開
  → Deriver          derive 宣言を手書きと同形の impl へ合成
  → ContextDesugar   context ブロックを pure / flatMap へ脱糖
  → TraitEncoder     trait / impl を辞書渡しへ脱糖
  → ChurchEncoder    data / match / fold を Church encoding 化
  → Generator        ARM64 アセンブリ生成
```

全フェーズが単一の GADT 風 AST（`AST.scala`）を高階再帰スキーム（`HFix` / `HCofree`）で共有し、
ユーザー起因のエラーは構造化された `CompileError` として型検査までに報告されます。各フェーズの設計（制約の表現、module、パターンの脱糖、エラー表示など）は [`CLAUDE.md`](CLAUDE.md) にまとめています。

## リポジトリ構成

```
src/main/scala/   コンパイラ本体
src/test/scala/   テスト（munit）
stdlib/           標準ライブラリ（import "std/..." で解決）
runtime/ffi.c     foreign で参照する C 関数の実装
benchmarks/       フィボナッチによる他言語比較
main.lam          サンプルプログラム（trait / context / 文字列埋め込み）
```