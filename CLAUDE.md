# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 概要

Scala 3 で実装された、小さな型付きラムダ計算系言語（System Fω 相当 + trait）のコンパイラ。`.lam` ソースを型検査し、ARM64/macOS 向けアセンブリを生成して clang でリンクする。言語仕様の詳細は `README.md`（日本語）を参照。

## コマンド

```bash
sbt compile                # コンパイル
sbt test                   # 全テスト（munit）。EndToEndSuite は .lam をコンパイル→リンク→実行して出力を確かめる（ARM64 macOS のみ）
./run.sh                   # main.lam をコンパイル→clang でリンク→実行（終了コード表示）
sbt "run <src.lam> <out.s>"   # 任意のソースをアセンブリへコンパイル（デフォルト: main.lam → build/out.s）
clang -arch arm64 build/out.s runtime/ffi.c -o build/out   # 手動リンク
```

実行には ARM64 macOS と clang が必要。

## アーキテクチャ

### コンパイルパイプライン（main.scala）

```
ParserAST (cats-parse) → ImportResolver → TAnalyser (型検査)
  → ModuleDesugar (module を平らな宣言列へ戻し、opaque type を表現へ読み替える)
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
`impl rec` と書いた impl だけ、メソッド本体から自身のインスタンスが見え、辞書は `let rec` に落ちる。再帰は `let rec` / `data rec` / `impl rec` の印がある所に限り、`rec` を使った所の停止性は保証しない（`data rec` の正の出現検査も行わない）。
自身を参照する `let rec` の値は関数（λ）に限る。文脈のない `impl rec` で自身のインスタンスを使うメソッドも同じ（初期化前の値を読まないため）。
trait `C` は辞書のデータ型でもあり（`Env.dataTypes` に登録）、TraitEncoder は `InstArrow` / `InstAbs` / `InstApp` を通常の `→` / `λ` / 適用に置き換え、trait / impl を辞書のデータ型と値に落とすだけで、解決もユーザーエラーも持たない。

### module（名前空間・private・opaque）

`module M { … }` の中の宣言 `x` は、完全な名前 `M.x` で定義される（`Env.values` などの定義表は完全な名前で引く）。ソースに書かれた名前は TAnalyser が `Env.resolveValue` / `resolveType` で完全な名前に直し、型検査後の AST は完全な名前だけを持つ:

- `Env.scope` が「修飾なしで書ける module メンバー（短い名前 → 完全な名前）と、見えている module」、`Env.members` が検査中の module の公開メンバー、`Env.modules` が宣言済みの module の公開メンバー。局所的な束縛（λ・let・型変数・式の中の `type` / `data`）は同名のメンバーを隠す
- `private` / `opaque` は宣言を包むノード `TopModified`。`private` は `members` に載せないだけ、`opaque data` は構成子を `members` から外すだけ
- `opaque type` は module の中では通常の別名（展開される）。module を閉じるとき（`sealDecls`）に `Env.abstractTypes` へ移し、中の宣言が Env に足した定義（値の型・別名・データ型・trait）を、型付きの宣言の注釈から別名を展開しない形で読み直す。そのため宣言の検査は「型付きの宣言」と「Env に足す定義」（`aliasOf` / `dataOf` / `traitOf`）に分かれている。インスタンスは読み直さず、module の中で登録した形のまま全体で有効
- ModuleDesugar は module / private / opaque / open を外して宣言を平らに並べ、抽象型を表現へ読み替える（注釈・型ノード・Env）。impl のターゲットだけはインスタンスを引く鍵なので抽象型の名前のまま残す。以降のフェーズに module と抽象型は残らない

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
- Generator: 束縛はすべてヒープの環境フレームに置き、`[x29, -16]` が現在のフレームを指す。関数本体の末尾位置の呼び出し（`Env.tail`）はフレームを畳んでジャンプにするので、末尾再帰はスタックを消費しない。メモリは `malloc` するだけで解放しない
- `src/test/lam/*.lam` — EndToEndSuite が実行するプログラム（期待する出力はスイート側に書く）。言語にコメント構文はない
- パッケージは `com.yuuki14202028`（sbt の `idePackagePrefix`）。ソースは `src/main/scala/` 直下に置き、パッケージ宣言のみ行う

## 規約

- ドキュメント・コミットメッセージは日本語。コミットは gitmoji prefix（`:sparkles:` 等）を使う