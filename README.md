# Lambda

Scala 3 で実装した、小さな型付きラムダ計算系言語（System Fω 相当 + trait）のコンパイラです。
`.lam` ソースを型検査し、ARM64/macOS 向けアセンブリを生成して clang でリンクします。

## 特徴

言語の説明は [`docs/language.md`](docs/language.md) にあります（コード例はすべてテストでコンパイルしています）。

- **型はすべて書く**: System Fω（値のラムダ、型の抽象と適用、型レベルの関数とカインド）。型推論はありません
- **代数的データ型とパターン**: `data` と `match`。入れ子のパターン、リテラル、選択肢、条件、束縛での分解。網羅性を検査します
- **再帰は `rec` と書いた所だけ**: `rec let` / `rec data` / `rec impl`、相互再帰は `rec { … }`
- **trait**: 型クラス。スーパークラス、デフォルトメソッド、高階カインド、量化された制約、`derive`
- **インスタンスを明示的に扱える**: `summon`、式としての `impl`、`(with d)`、`using`
- **context 記法**: モナドの計算を順に並べる書き方
- **module**: 名前空間、`private`、`opaque type` / `opaque data`
- **エラー表示**: 原因の部分に下線を引き、関係する位置と直し方を添えます
- **C FFI**: `foreign[T → U] name` で C の関数を参照します
- **コード生成**: 関数値はコードポインタ + 環境ポインタのクロージャとしてコンパイルされます。末尾位置の呼び出しはジャンプになるので、末尾再帰でループを書けます（メモリは確保するだけで解放しません）

組み込み型は次のとおりです。

```text
i8 i16 i32 i64 isize  u8 u16 u32 u64 usize  f32 f64
char bool unit
foreign.C.String  foreign.C.VoidPtr  foreign.C.Ptr[T]
```

`+` や `==` などの演算子は、`infixl 6 + = add` のように宣言された関数の呼び出しです（`a + b` は `add[T](a)(b)`、`T` は `a` の型）。
`stdlib/operators.lam` が演算子ごとの trait（`Add` / `Eq` / `Ord` など）と組み込みの型のインスタンスを宣言しているので、演算子を使うには `import "std/operators"` が必要です。自分の型では、その trait を実装します。記号・優先順位・結合性は宣言で決まるので、`infixr 5 :: = cons` のように独自の演算子も作れます。`&&` と `||` だけは `bool` 専用の組み込みです。

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

互いに無関係な宣言のエラーはまとめて報告します（失敗した宣言を使う側のエラーは省きます）。似た名前の候補、`rec` の付け忘れ、後で宣言されている名前、`private` な名前、`opaque type` と表現の取り違え、足りない `with` 節、古い構文（`let rec` など）といった、よくある原因には直し方を示します。import したファイルの中のエラーは、そのファイルの位置で表示します。表示の例は `src/test/diagnostics/`（`.lam` と、表示されるエラーの `.txt`）にあります。

## アーキテクチャ

```
ParserAST (cats-parse)
  → ImportResolver   import の解決・展開
  → TAnalyser        型検査（trait / context / derive / module は構文を保ったまま型付け。制約と名前の解決もここで行う）
  → ModuleDesugar    module を平らな宣言列に戻し、opaque type を表現へ読み替える
  → RecordDesugar    名前付きフィールドを読む関数（アクセサ）を生成
  → Deriver          derive 宣言を手書きと同形の impl へ合成
  → ContextDesugar   context ブロックを pure / flatMap へ脱糖
  → PatternDesugar   match と束縛のパターンを、構成子 1 段の分岐と if / let へ展開
  → SelectDesugar    ドット記法 x.f を、x の型の module の関数か trait のメソッドの適用へ
  → OperatorDesugar  演算子を、宣言された関数の適用へ（&& と || は if へ）
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
main.lam          サンプルプログラム（演算子の trait / context / パターン / 文字列埋め込み）
examples/         まとまった量のサンプル
  calculator.lam    式の電卓（module の中の構文解析器、相互再帰、パターン、context、デフォルトメソッド）
  transformer.lam   モナド変換子（高階カインド、量化されたスーパークラス）
  ranking.lam       成績の集計（順序の辞書を作って summon / (with d) / using で差し替える、入れ子の module、derive）
  json.lam          JSON 風の値の読み書き（リストや組を通した再帰、文字列、入れ子パターン、Option の context）
```