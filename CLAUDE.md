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

制約・スーパークラスは宣言末尾の `with C[a]` 節（let は返り値注釈の後）。`derive Functor[D]` / `derive Foldable[D]` は
TAnalyser が検査して InstanceDef を Env に登録し、Deriver が手書き impl と同形の TopImpl を合成する（Deriver.scala）。

エラー処理は 3 層に分かれる（Effects.scala / CompileError.scala）:

- ユーザー起因のエラーは、エラー種別ごとに case を持つ構造化 ADT `CompileError` を `EitherS[A] = Either[CompileError, A]` で伝播する。case はメッセージ文字列ではなく型・名前・カインド等のデータを持ち、表示は `CompileError.render` に一元化されている（多言語対応は render の差し替えで行う）
- `Env` を読む処理は共通スタック `Check[A] = ReaderT[EitherS, Env, A]` に統一されている。フェーズ内ヘルパーは `Check` を通し、`.run(env)` はフェーズ境界（宣言単位の StateT・公開 API）でのみ呼ぶ
- コンパイラ不変条件違反（前段フェーズのバグでありユーザーには直せない失敗）は Either では運ばず、各フェーズの `invariant` ヘルパー（`sys.error`）で即座に落とす。このため ChurchEncoder は `Reader[DataEnv, _]` 1 段、Generator は `ReaderT[State[GenState, _], Env, _]` で Either 層を持たない

### AST 表現（最重要）

全フェーズが単一の GADT 風 `AST[R[_], I]`（AST.scala）を共有する。`I` はノード種別（`Expr` / `Type` / `Decl` / `Program.type`）のインデックス、`R[_]` は再帰位置の表現。高階再帰スキームで結ばれる:

- `Rec[I] = HFix[AST, I]` — 素の AST
- `IndexedRec[I] = HCofree[AST, Int, I]` — Offset付き AST
- `TypeRec[I] = HCofree[AST, TypeAnn, I]` — 型注釈付き AST（TAnalyser 以降）。`.extract` で注釈、`.project` でノードを取得
- `HFunctor`（HFunctor.scala）が `hmap` による自然変換を提供

スマートコンストラクタは AST.scala に定義されており、`Rec` 用は小文字名（`app`, `arrow` など）、`TypeRec` 用は末尾 `T`（`appT`, `programT` など）。AST を構築・変換するときは必ずこれらを使う。

### フェーズ間で共有される環境

`Env`（Env.scala）が値の型・型変数のカインド・型別名・データ型定義・trait/インスタンス定義をすべて保持し、`ProgramAnn` 経由で後段フェーズへ受け渡される。

### その他

- `stdlib/*.lam` — `import std/...` で解決される標準ライブラリ。`StandardLibrary.scala` が演算子名（`add` 等）を組み込み演算へ対応付ける
- `runtime/ffi.c` — `foreign` で参照する C 関数の実装。関数値はコードポインタ + 環境ポインタのクロージャ
- パッケージは `com.yuuki14202028`（sbt の `idePackagePrefix`）。ソースは `src/main/scala/` 直下に置き、パッケージ宣言のみ行う

## 規約

- ドキュメント・コミットメッセージは日本語。コミットは gitmoji prefix（`:sparkles:` 等）を使う