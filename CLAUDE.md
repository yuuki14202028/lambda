# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 概要

Scala 3 で実装された、小さな型付きラムダ計算系言語（System Fω 相当 + trait）のコンパイラ。`.lam` ソースを型検査し、ARM64/macOS 向けアセンブリを生成して clang でリンクする。言語仕様の詳細は `README.md`（日本語）を参照。

## コマンド

```bash
sbt compile                # コンパイル
./run.sh                   # main.lam をコンパイル→clang でリンク→実行（終了コード表示）
sbt "run <src.lam> <out.s>"   # 任意のソースをアセンブリへコンパイル（デフォルト: main.lam → build/out.s）
clang -arch arm64 build/out.s runtime/ffi.c -o build/out   # 手動リンク
```

実行には ARM64 macOS と clang が必要。

## アーキテクチャ

### コンパイルパイプライン（main.scala）

```
ParserAST (cats-parse) → ImportResolver → TAnalyser (型検査)
  → Deriver (derive 宣言を型付き TopImpl へ合成) → ContextDesugar (context ブロックを bind/ret へ脱糖)
  → TraitEncoder (辞書渡しへ脱糖) → ChurchEncoder (data/match/fold を Church encoding 化)
  → Generator (ARM64 アセンブリ生成)
```

制約・スーパークラスは宣言末尾の `with C[a]` 節（let・trait メソッド・impl メソッドは返り値注釈の後）。`derive Functor[D]` / `derive Foldable[D]` は
TAnalyser が検査して InstanceDef を Env に登録し、Deriver が手書き impl と同形の TopImpl を合成する（Deriver.scala）。

### 制約の表現（Lean 4 風）

制約は「辞書 `C[τ̄]` を受け取る、インスタンス解決で埋まる引数」として型と項に現れる。構文は `with` 節だけで、パーサが次のノードへ展開する:

- 型 `InstArrow`: `[Show[a]] → a → String`（`let f[a](x: a): String with Show[a]` の型は `∀a. [Show[a]] → a → String`。trait メソッド `show` は `∀a. [Show[a]] → a → String`）
- 抽象 `InstAbs`: `λ[Show[a]]. e`（本体でその制約がスコープに入る。`Env.givens`）
- 適用 `InstApp`: 式の型が `[C[τ̄]] → τ` になった時点で TAnalyser が `Resolver` で制約を解決し、根拠（`Evidence`）を記録して挿入する

解決は型検査の一部で、その時点までに宣言されたインスタンスとスコープ中の制約だけを見る（インスタンスも「宣言してから使う」。impl 自身のメソッド本体からそのインスタンスはまだ見えない）。
trait `C` は辞書のデータ型でもあり（`Env.dataTypes` に登録）、TraitEncoder は `InstArrow` / `InstAbs` / `InstApp` を通常の `→` / `λ` / 適用に置き換え、trait / impl を辞書のデータ型と値に落とすだけで、解決もユーザーエラーも持たない。

エラー処理は 3 層に分かれる（Effects.scala / CompileError.scala）:

- ユーザー起因のエラーは、エラー種別ごとに case を持つ構造化 ADT `CompileError` を `EitherS[A] = Either[CompileError, A]` で伝播する。case はメッセージ文字列ではなく型・名前・カインド等のデータを持ち、表示は `CompileError.render` に一元化されている（多言語対応は render の差し替えで行う）
- `Env` を読む処理は共通スタック `Check[A] = ReaderT[EitherS, Env, A]` に統一されている。フェーズ内ヘルパーは `Check` を通し、`.run(env)` はフェーズ境界（宣言単位の StateT・公開 API）でのみ呼ぶ
- コンパイラ不変条件違反（前段フェーズのバグでありユーザーには直せない失敗）は Either では運ばず、各フェーズの `invariant` ヘルパー（`sys.error`）で即座に落とす。ユーザーエラーを出すのは TAnalyser（とそこから呼ばれる KAnalyser / Resolver / Deriver.check / ContextDesugar.desugarContext）までで、Deriver.desugar 以降の脱糖フェーズと Generator は Either 層を持たない
- TAnalyser では、式と型は代数 `tcAlg`、宣言は `declRule` が検査する。`declRule` は型付きの宣言と「後続の宣言から見える Env」を同時に返す。top-level 宣言と in 式（`data` / `type` / `let` / `match` と `fold`）の検査は `checkData` / `checkAlias` / `checkBinding` / `checkCases` を共有する

### AST 表現（最重要）

全フェーズが単一の GADT 風 `AST[R[_], I]`（AST.scala）を共有する。`I` はノード種別（`Expr` / `Type` / `Decl` / `Program.type`）のインデックス、`R[_]` は再帰位置の表現。高階再帰スキームで結ばれる:

- `Rec[I] = HFix[AST, I]` — 素の AST
- `IndexedRec[I] = HCofree[AST, Int, I]` — Offset付き AST
- `TypeRec[I] = HCofree[AST, TypeAnn, I]` — 型注釈付き AST（TAnalyser 以降）。`.extract` で注釈、`.project` でノードを取得
- `HFunctor`（HFunctor.scala）が `hmap` による自然変換を提供

スマートコンストラクタは AST.scala に定義されており、`Rec` 用は小文字名（`app`, `arrow` など）、`TypeRec` 用は末尾 `T`（`appT`, `programT` など）。AST を構築・変換するときは必ずこれらを使う。脱糖フェーズで型付きの式を組み立てるときは、子の型から注釈を導出する `mkAbs` / `mkTyAbs` / `mkApp` / `mkTyApp` を使う。

### フェーズ間で共有される環境

`Env`（Env.scala）が値の型・型変数のカインド・型別名・データ型定義・trait/インスタンス定義・スコープ中の制約をすべて保持し、`ProgramAnn` 経由で後段フェーズへ受け渡される。

### その他

- `stdlib/*.lam` — `import std/...` で解決される標準ライブラリ。`StandardLibrary.scala` が演算子名（`add` 等）を組み込み演算へ対応付ける
- `runtime/ffi.c` — `foreign` で参照する C 関数の実装。関数値はコードポインタ + 環境ポインタのクロージャ
- パッケージは `com.yuuki14202028`（sbt の `idePackagePrefix`）。ソースは `src/main/scala/` 直下に置き、パッケージ宣言のみ行う

## 規約

- ドキュメント・コミットメッセージは日本語。コミットは gitmoji prefix（`:sparkles:` 等）を使う