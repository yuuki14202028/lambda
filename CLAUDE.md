# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 概要

Scala 3 で実装した、小さな型付きラムダ計算系言語（System Fω 相当 + trait）のコンパイラ。`.lam` ソースを型検査し、すべてをラムダ式に落としてから、インタプリタで実行するか、ARM64/macOS 向けアセンブリを生成する。言語の説明は `docs/language.md`。

実験・学習用のコードなので、**わかりやすさを優先する**。木は段階ごとの素の enum、処理はパターンマッチと明示的な再帰で書く（高階の再帰スキームやモナド変換子は使わない）。コメントは「なぜそうなっているか」が読んでもわからない所だけ。

## コマンド

```bash
sbt compile
sbt test                       # 全テスト（munit）
sbt "run <src.lam>"            # コンパイルして、インタプリタで実行する
sbt "run <src.lam> <out.s>"    # ARM64 のアセンブリを書き出す
./run.sh                       # main.lam をコンパイル → clang でリンク → 実行（ARM64 macOS と clang が必要）
UPDATE_DIAGNOSTICS=1 sbt "testOnly *DiagnosticSuite"   # エラー表示のスナップショットを書き直す
```

## パイプライン（Compiler.scala）

```
Lexer → Parser (Surface) → Imports → Resolve (Resolved) → Declarations / Check (Typed)
  → Lower (Core。CoreCheck が検査する) → Encode (Lambda) → Interpreter / Generator
```

| 段階 | 木 | すること |
| --- | --- | --- |
| `Parser` | `Surface` | 再帰下降。名前は綴りのまま、演算子は並びのまま。適用は「頭 + 引数の並び」の 1 ノード |
| `Imports` | `Surface` | `import` をファイルの宣言に置き換える（top-level では同じファイルは一度。module の中は module ごと） |
| `Resolve` | `Resolved` | スコープだけで決まることを全部決める（下の「名前解決」） |
| `Declarations` / `Check` | `Typed` | 型検査。宣言は `Declarations`、式は `Check`、型の変換は `Types`、適用は `Application`、インスタンス解決は `Instances`、derive は `Derive` |
| `Lower` | `Core` | 糖衣を外す。制約は普通の引数に、trait は辞書のデータ型に、opaque type は中身に、決定木は 1 段の match と if に |
| `CoreCheck` | — | Core の型検査。通らなければコンパイラのバグ（ユーザーエラーではない） |
| `Encode` | `Lambda` | data / match を関数に符号化し、型を消す。**すべてをラムダ式で表す**のが目標なので、これより後にデータ型はない |
| `Interpreter` | — | Lambda をそのまま評価する。言語の意味の基準 |
| `Generator` | — | Lambda から ARM64 のアセンブリ（クロージャ変換、末尾呼び出しはジャンプ） |

### 改行の規則（Parser）

式は改行で終わる。ただし、二項演算子・`=`・`=>`・λ の `.` の後、括弧の中、`in` / `then` / `else` / 閉じ括弧 / `.名前` の前では改行できる。行を二項演算子で始めることはできない。適用の引数は関数に続けて書く（`f(x)`、`f[T]`。トークンの `spaceBefore` で判定する）。宣言は改行か `;` で区切る。`name[…]` は型引数とも名前付きのリテラルの要素とも読めるので、両方の読みを持ち（`Argument.Bracket`）、名前解決が決める。

### 名前解決（Resolve.scala）

- 名前は `Symbol`（同一性で比べる。同じ綴りでも別の束縛なら別の Symbol）になる。module のメンバーの Symbol は、完全な名前（`List.length`）を表示用に持つ
- `Names` が名前の表（値・型・module・演算子・interpolate・リテラル）。スコープにも module のメンバー一覧（`Module.exported` / `hidden`）にも使う。宣言は書かれた順に処理するので、その時点までの名前だけが見える
- `module M { … }` は同じ名前でもう一度開ける（`Module` は可変）。`private` と `opaque data` の構成子は `hidden` に入り、その module の中からだけ見える。`open` はメンバーをスコープに足す
- 演算子の並びは、スコープにある宣言の優先順位と結合性で木に組む（`climb`）。`&&` と `||` だけは組み込み
- パターンの引数のない名前は、スコープに構成子があれば構成子、なければ変数。`[p, q]` / `(p, q)` は、宣言された関数（構成子）のパターンに読み替える
- `a.b.c` は、全体が修飾された名前ならその値。そうでなければ、値を指すいちばん長い前半を値として、残りをドット記法（`Select`）にする
- ドット記法 `x.f` の `f` は型で決まるので、`Select` は候補（`Candidates`: 型ごとの module のメンバーと、スコープにある trait のメソッド）を持つ。型の module は、型と同じ場所にある同じ名前の module（組み込みの型はその名前、文字列は `String`）
- 名前付きフィールドを読む関数は、型の module のメンバーとして Symbol を作る
- 解決に失敗した宣言は結果から外すが、宣言した名前は残る（使う側は「失敗した宣言に依存」として黙る）。エラーの手がかり（似た名前・後ろで宣言・`rec` が要る・private）もここで付ける

### 型（Type.scala）

- 型の中の束縛変数は番号（`Bound`）、宣言の型引数や項の Λ が束縛する変数は `Symbol`（`Var`）。常に正規形で持ち（`Type.apply` が β 簡約する）、等しさは `==`
- **制約は辞書の型**: `Show[a]` は辞書の型そのもの。量化された制約 `∀a. Show[a] ⇒ Show[f[a]]` は `∀a. [Show[a]] → Show[f[a]]`。`Given(c, t)`（表示は `[c] → t`）は「c の辞書を解決して渡してもらう関数」の型で、`with C[a]` が付いた束縛の型はこれになる
- opaque type は、宣言した module のブロックの中では別名、閉じた後は中身のわからない型変数（`Var`）

### 型引数を読む規則（Application.scala）

型は下から合成する（期待される型を上から渡さない）。適用の並び `head[T](v)…` を左から処理する:

- 値の引数: 先頭の ∀ と制約を外して `A → …` にし、`A` と値の型の一方向の照合（`Types.matchType`）で未決定の型パラメータを決める
- 書かれた `[T]`: 未決定の最初の型パラメータを決める（なければ ∀ を 1 つ外す）
- `(with d)`: まだ辞書が渡されていない最初の制約に渡す
- 並びの終わり: 先頭に残った制約を外す。未決定の型パラメータが残ればエラー（`TypeArgumentUndetermined`）。辞書が渡されなかった制約を `Instances.resolve` で解決する

名前だけの参照も引数のない適用。演算子・ドット記法（受け手が最初の引数）・リテラル・組・文字列埋め込み・context は、宣言された関数の適用としてここを通る。リストリテラルだけは、`cons` の型引数を最初の要素から読んで `nil` にも同じ型引数を渡す。糖衣は `Typed.Sugar(形, 意味)` として残し、`Lower` が意味を使う。

### インスタンス解決（Instances.scala）

使える辞書（スコープにある制約と、宣言されたインスタンス）はどれも型が `∀ā. [前提…] → C[T̄]`。頭 `C[T̄]` が求める制約に合えば、前提を同じように解決して渡す。探す順は、スコープにある制約 → インスタンス（trait と引数の先頭の形 `InstanceKey` ごとに 1 つ）→ スコープにある制約のスーパークラス。量化された制約を示すときは、∀ の変数と前提をスコープに入れて頭を解決する。結果は辞書を表す `Typed` の式で、型には影響しない。

### 宣言の検査（Declarations.scala）

- 宣言は「シグネチャ（注釈）」と「本体」に分けて検査し、本体が失敗してもシグネチャは環境に残す。エラーは宣言ごとに集める（`CompileFailure` を宣言の境界で捕まえる。impl の各メソッドと rec の組の各メンバーも別々に集める）
- trait `C[p̄] with S̄` は辞書のデータ型でもある: `data C[p̄] = { MkC(S̄ の辞書…)(メソッド…) }`。メソッドの型は `∀p̄. [C[p̄]] → シグネチャ`。デフォルトの本体は `∀p̄. [S̄] → 必須メソッド… → シグネチャ` の関数になり、impl で省略されたメソッドをその適用で埋める
- impl は辞書の束縛になる（`Λā. λ[文脈]. MkC[T̄](…)`）。インスタンスも「宣言してから使う」。`rec impl` / `rec { … }` なら組のインスタンスが本体から見える。文脈のない辞書はすぐに作られるので、スーパークラスは組の中で先に宣言されたものに限り、組の辞書を読むメソッドは関数に限る
- `derive` は、データ型の形から普通の impl（`Resolved.Impl`）を作って同じ検査に通す。導出したインスタンスは自分のメソッドから見える
- module のブロックを閉じるとき、その opaque type を型変数にして、ブロックの宣言が足した型を注釈から読み直す（`seal` / `reread`）。Typed の中身は表現のままで、`Lower` が opaque type を表現に読み替える

### match（Check.scala / Patterns.scala）

パターンを検査して `CheckedPattern` にし、行列から決定木（`Decision`）を作る。`p | q` は同じ分岐を指す行に分け、条件付きの分岐は「偽なら後続へ」の葉になる（網羅には数えない）。リテラルは `==` に宣言された関数で比べる（bool だけは分岐）。網羅性と到達不能は木から調べる。`Lower` は、複数の葉から着く本体を関数として一度だけ置く。束縛でのパターン・context の束縛・`fold` も同じ仕組みを使う。

### エラー（Errors.scala / Diagnostics.scala）

- ユーザーエラーは `CompileError`（case ごとに型や名前を持つ）。`message` は 1 行の要約で、テストはこれか case を確かめる。表示の文言は `Diagnostics` にだけある（見出し・主な位置の説明・関係する位置・note・help）
- 関係する位置（`Related`: 注釈、関数の型、最初の分岐など）と手がかり（`Hint`）は、`CompileError.Detailed` で包んで付ける。`core` で外せる
- コンパイラの不変条件の違反は `sys.error("Compiler invariant violation: …")`
- 表示を変えたら、スナップショット（`src/test/diagnostics/*.txt`）を書き直して差分を確認する

### 実行（Encode / Interpreter / Generator）

- 符号化: 構成子は `λフィールド…. λ構成子ごとの関数…. 自分の関数(フィールド…)`、match は値に分岐の関数を渡す。フィールドのない構成子の関数は unit を受け取る
- 値はすべて 64 ビットの 1 語。`foreign` の関数は `runtime/ffi.c` にあり、インタプリタは同じ名前の実装を持つ（`Interpreter.foreign`）。C の関数を足すときは両方に足す
- Generator: クロージャは `[コードのアドレス, 捕まえた変数…]`。rec の変数はセル経由。末尾位置の呼び出しはフレームを畳んでジャンプ。メモリは解放しない。深い再帰のために、`_main` で大きなスタックに切り替える

## テスト

- `EndToEndSuite`: `examples/`・`main.lam`・`stdlib_test.lam`・`src/test/lam/*.lam` を、インタプリタと ARM64 の両方で実行して出力を確かめる
- `DocSuite`: `docs/language.md` の `lam` のコード例をコンパイルする。構文や標準ライブラリを変えたら文書も直す
- `DiagnosticSuite`: エラー表示のスナップショット
- そのほかのスイートは機能ごと。`Lam`（テスト用の入口）でソースをコンパイル・実行する

## 規約

- 標準ライブラリ（`stdlib/*.lam`、`import "std/…"`）: 型ごとの関数は、その型の module に、値を最初の引数に取る形で置く（ドット記法で呼べるように）。trait のメソッドと同じ名前の関数は module に置かない（ドット記法が曖昧になる）
- パッケージは `com.yuuki14202028`。ソースは `src/main/scala/` 直下に置く
- ドキュメント・コミットメッセージは日本語。コミットは gitmoji prefix（`:sparkles:` 等）を使う
