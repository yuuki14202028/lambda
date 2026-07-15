# Lambda

Scala 3 で実装した、小さな型付きラムダ計算系言語（System Fω 相当 + trait）のコンパイラです。
`.lam` ソースを型検査し、ARM64/macOS 向けアセンブリを生成して clang でリンクします。

## 特徴

- **System Fω**: 値のラムダ `λx: T. e`、型抽象 `ΛA. e`、型適用 `f[T]`、型レベルラムダとカインド（`*`, `* → *`）。型注釈と型適用はすべて明示で、HM 型推論は行いません
- **代数的データ型**: `data` / 再帰型は `data rec`。`match` は全コンストラクタの網羅が必須、`fold` で再帰型を畳み込みます。バックエンドでは Church encoding に変換されます
- **trait**: `trait` / `impl` による型クラス。高階カインドの trait（`Monad[m: * → *]` など）、`with C[a]` 節（宣言末尾）による制約・スーパークラスに対応。型適用が明示なので辞書解決は決定的で、辞書渡しコードへ脱糖されます
- **derive**: `derive Functor[Option]` / `derive Foldable[List]` で、データ型の構造から `map` / `foldRight` / `foldLeft` のインスタンスを自動導出します。導出対象は構造から一意に決まる trait（正準シグネチャと α 同値なもの）に限り、負の位置の出現や畳み込めないフィールドはコンパイルエラーになります
- **context 記法**: do 記法相当。`context[M] { x: T = e; ...; result }` を `Monad` trait の `pure` / `flatMap` 呼び出しへ脱糖します
- **糖衣構文**: 関数定義 `let f[A](x: T)(y: U): R = e`、ブロック式 `{ e; ...; result }`、文字列埋め込み `` `result = {compute()}\n` ``、2 引数関数の中置適用 `120 safeDiv 5`
- **モジュール**: `import "std/operators"` は同梱の `stdlib/` を、それ以外のパスはソースファイルからの相対で解決します
- **C FFI**: `foreign[T → U] name` で C 関数を参照。実装は `runtime/ffi.c`
- 関数値はコードポインタ + 環境ポインタのクロージャとしてコンパイルされます

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
./calculator.sh   # デモ: 対話式電卓（calculator.lam）

sbt "run <src.lam> <out.s>"                              # 任意のソースをアセンブリへ
clang -arch arm64 build/out.s runtime/ffi.c -o build/out # 手動リンク

sbt test                        # 全テスト（munit）
./benchmarks/run_fibonacci.sh   # 他言語とのベンチマーク比較
```

## アーキテクチャ

```
ParserAST (cats-parse)
  → ImportResolver   import の解決・展開
  → TAnalyser        型検査（trait / context / derive は構文を保ったまま型付け）
  → Deriver          derive 宣言を手書きと同形の impl へ合成
  → ContextDesugar   context ブロックを pure / flatMap へ脱糖
  → TraitEncoder     trait / impl を辞書渡しへ脱糖
  → ChurchEncoder    data / match / fold を Church encoding 化
  → Generator        ARM64 アセンブリ生成
```

全フェーズが単一の GADT 風 AST（`AST.scala`）を高階再帰スキーム（`HFix` / `HCofree`）で共有し、
エラーは `Either[String, A]` で伝播します。各フェーズの設計はリポジトリ直下のドキュメントを参照してください。

| ドキュメント | 内容 |
| --- | --- |
| [`TypeCheck.md`](TypeCheck.md) | TAnalyser（型注釈必須、HM 推論なし） |
| [`SystemFOmega.md`](SystemFOmega.md) | カインドと型レベル計算 |
| [`DataType.md`](DataType.md) / [`ChurchEncode.md`](ChurchEncode.md) | data / match の Church encoding 化 |
| [`Trait.md`](Trait.md) | trait / impl の辞書渡し脱糖 |

## リポジトリ構成

```
src/main/scala/   コンパイラ本体
src/test/scala/   テスト（munit）
stdlib/           標準ライブラリ（import "std/..." で解決）
runtime/ffi.c     foreign で参照する C 関数の実装
benchmarks/       フィボナッチによる他言語比較
main.lam          サンプルプログラム（trait / context / 文字列埋め込み）
calculator.lam    サンプルプログラム（再帰データ型と fold による電卓）
```