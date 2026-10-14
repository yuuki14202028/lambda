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
  → RecordDesugar (名前付きフィールドのアクセサを生成し、名前付きの構築を位置の適用に直す)
  → Deriver (derive 宣言を型付き TopImpl へ合成) → ContextDesugar (context ブロックを bind/ret へ脱糖)
  → PatternDesugar (match と束縛のパターンを、構成子 1 段の分岐と if / let へ展開する)
  → SelectDesugar (ドット記法 x.f を、型の module の関数か trait のメソッドの適用へ展開する)
  → OperatorDesugar (演算子を、宣言された関数の適用へ展開する。&& と || は if へ)
  → TraitEncoder (辞書渡しへ脱糖) → ChurchEncoder (data/match/fold を Church encoding 化)
  → Generator (ARM64 アセンブリ生成)
```

制約・スーパークラスは宣言末尾の `with C[a]` 節（let・trait メソッド・impl メソッドは返り値注釈の後）。`derive Functor[D]` / `derive Foldable[D]` / `derive Traversable[D]` / `derive Eq[D]` / `derive Ord[D]` / `derive Show[D]` は
TAnalyser が検査して InstanceDef を Env に登録し、Deriver が手書き impl と同形の TopImpl を合成する（Deriver.scala）。trait は名前で選び、正準シグネチャと照合する。
Functor などは最後の型パラメータについて `impl[p̄] C[D[p̄]]`。Eq / Ord / Show は `impl[ā] C[D[ā]] with C[a₁], …`（型パラメータはカインド * に限る）で、フィールドのインスタンスはその文脈の下で解決し（`FieldCompare`）、D[ā] そのもののフィールドはメソッドの中の再帰関数で扱う（`rec` なしで自己再帰の型に使える）。Eq は構造の比較、Ord は構成子の宣言順→フィールドの辞書順（スーパークラス Eq のメソッドを使う）、Show は `C(name = 値, 値)`（文字列の連結は foreign の concat）。

### 制約の表現（Lean 4 風）

制約は「辞書 `C[τ̄]` を受け取る、インスタンス解決で埋まる引数」として型と項に現れる。構文は `with` 節だけで、パーサが次のノードへ展開する:

- 型 `InstArrow`: `[Show[a]] → a → String`（`let f[a](x: a): String with Show[a]` の型は `∀a. [Show[a]] → a → String`。trait メソッド `show` は `∀a. [Show[a]] → a → String`）
- 抽象 `InstAbs`: `λ[Show[a]]. e`（本体でその制約がスコープに入る。`Env.givens`）
- 適用 `InstApp`: 式の型が `[C[τ̄]] → τ` になった時点で TAnalyser が `Resolver` で制約を解決し、根拠（`Evidence`）を記録して挿入する

制約 `Constraint(name, arg, params, premises)` は量化された制約 `∀params. premises ⇒ name[arg]` も表す（`with ∀a. Show[a] ⇒ Show[f[a]]`。前提にも量化された制約を書け、スーパークラスにも書ける）。辞書の型は `∀params. 前提の辞書… → name[arg]`（文脈付きインスタンスの辞書と同じ形）で、`params` は制約の中だけの束縛子として `substType` / `freeTypeVars` / `Equivalence.alpha` が扱う。
スコープ中の量化された制約（スコープ中の制約のスーパークラスとして得られるものを含む。`Resolver.superClosure`）はインスタンスのように使われ（根拠 `Evidence.Apply`）、量化された制約を示すときは `params` と前提をスコープに入れてヘッドを解決する（根拠 `Evidence.Abs`。辞書は `Λparams. λ前提の辞書. …`）。

trait のメソッドはデフォルトの本体を持てる（`MethodSig.body`、`TraitDef.defaults`）。本体は、必須メソッド（デフォルトを持たないメソッド）を trait の型パラメータでの普通の値として、スーパークラスの制約を givens としてスコープに入れて検査する。自身のインスタンスも他のデフォルトも見えないので、辞書は自分自身を参照しない（`rec` 不要）。TraitEncoder はデフォルトを `$default_C_m: ∀p̄. スーパークラスの辞書… → 必須メソッド… → シグネチャ` に落とし、impl で省略されたメソッドをその適用で埋める（そのときだけ、定義されたメソッドを `$method_i` に束縛する）。

インスタンスは明示的にも扱える。`summon[C[τ̄]]`（`Summon` ノード）は解決した辞書そのもので、型は辞書の型 `C[τ̄]`。`f(with d)` はパーサが `InstApp(f, Evidence.Explicit(d))` として読み、型検査は `f` の位置だけ制約を解決せずに残して（`Env.keepConstraints`。`indexedAlg` が子ごとに切り替える）、先頭の制約を `d` で満たす。残った制約は従来どおり解決する。ドット記法の `x.f(with d)` と修飾された名前の `M.f[T](with d)` は、パーサが `(with d)` を `Select` / `Path` の `dictionaries` に取り込み、型検査が関数の制約を先頭から順に満たす（`satisfyConstraints`。残りは解決する）。式としての `impl C[τ̄] { … }`（`Instance` ノード）は辞書の値を作る。インスタンスとしては登録せず、メソッドの検査（`implBody`）と辞書の組み立て（`TraitEncoder.dictionaryValue`）は宣言の impl と共有する。`using d in e`（`Using` ノード）は `d` の型（辞書の型）から制約を読み取り（`constraintOfDictionary`）、`e` の中でスコープに入れる。スコープ中の制約は内側が優先される（`Env.withGivens` と TraitEncoder のスコープはどちらも内側を先頭に置く）。

解決は型検査の一部で、その時点までに宣言されたインスタンスとスコープ中の制約だけを見る（インスタンスも「宣言してから使う」。impl 自身のメソッド本体からそのインスタンスはまだ見えない）。
再帰は `rec` の印がある所に限る。`rec` は宣言の前に付ける修飾子で、宣言 1 つ（`rec let` / `rec data` / `rec impl`）か、同じ種類の宣言を囲んだ組（`rec { let …  let … }`）に付ける。組の全員が互いに、また自身から見える（相互再帰）。`derive` はインスタンス宣言なので `impl` と同じ組に入れられる（相互再帰のデータ型への `derive` は `rec { derive …  derive … }`）。AST では宣言を包む `TopRec(decls)`、式の中では `LetRec(bindings, body)` / `DataLetRec(datas, body)`、context ブロックの中では `ContextStep.Rec(bindings)`。
`rec` を使った所の停止性は保証しない（`rec data` の正の出現検査も行わない）。初期化前の値を読まないよう、関数（λ）でない `rec let` の値は組の誰も参照できず、文脈のない `rec impl` で組のインスタンスを使うメソッドも関数に限る。
`rec impl` のメソッド本体からは組のインスタンスが見え、辞書は `rec` の組の let に落ちる。`rec data` の組は `DataDef.group` に記録され、ChurchEncoder は組のデータ型に言及するフィールドを符号化せず名前のまま残す。Generator は組の全員のセルを先に確保してから値を入れる。`fold` は自身の型の出現だけを畳み込む（組の他の型は畳み込まない）。
trait `C` は辞書のデータ型でもあり（`Env.dataTypes` に登録）、TraitEncoder は `InstArrow` / `InstAbs` / `InstApp` を通常の `→` / `λ` / 適用に置き換え、trait / impl を辞書のデータ型と値に落とすだけで、解決もユーザーエラーも持たない。

### module（名前空間・private・opaque）

`module M { … }` の中の宣言 `x` は、完全な名前 `M.x` で定義される（`Env.values` などの定義表は完全な名前で引く）。ソースに書かれた名前は TAnalyser が `Env.resolveValue` / `resolveType` で完全な名前に直し、型検査後の AST は完全な名前だけを持つ:

- `Env.scope` が「修飾なしで書ける module メンバー（短い名前 → 完全な名前）と、見えている module」、`Env.members` が検査中の module の公開メンバー、`Env.modules` が宣言済みの module の公開メンバー。局所的な束縛（λ・let・型変数・式の中の `type` / `data`）は同名のメンバーを隠す
- `private` / `opaque` は宣言を包むノード `TopModified`。`private` は `members` に載せないだけ、`opaque data` は構成子を `members` から外すだけ
- `opaque type` は module の中では通常の別名（展開される）。module を閉じるとき（`sealDecls`）に `Env.abstractTypes` へ移し、中の宣言が Env に足した定義（値の型・別名・データ型・trait）を、型付きの宣言の注釈から別名を展開しない形で読み直す。そのため宣言の検査は「型付きの宣言」と「Env に足す定義」（`aliasOf` / `dataOf` / `traitOf`）に分かれている。インスタンスは読み直さず、module の中で登録した形のまま全体で有効
- 同じ名前の module はもう一度開ける（メンバーを足す）。前のブロックの公開メンバー（`Env.modules`）と private なメンバー（`Env.modulePrivates`）をスコープに戻して検査する。opaque type の表現が見えるのは、宣言したブロックの中だけ
- ModuleDesugar は module / private / opaque / open を外して宣言を平らに並べ、抽象型を表現へ読み替える（注釈・型ノード・Env）。impl のターゲットだけはインスタンスを引く鍵なので抽象型の名前のまま残す。以降のフェーズに module と抽象型は残らない

### ドット記法（x.f）

`x.f[T]` は「`x` の型の module にある関数 `f`」か「スコープにある trait のメソッド `f`」の呼び出し（Lean 4 の一般化フィールド記法に trait のメソッドを加えたもの。`selectTarget`）。両方にあれば曖昧さのエラー（`SelectAmbiguous`）。型の module は型と同じ完全な名前の module で（`moduleOfType`。組み込みの型はその名前、`foreign.C.String` は `String`。型変数・関数の型にはない）、メンバーは `Env.moduleMember` で引く。trait のメソッドは短い名前で書けるもので、局所的な束縛に隠されていないもの（名前の型がメソッドの型のまま。`traitMethodInScope`）。受け手の型を問わないので、型変数の受け手にも使え、インスタンスは型引数を渡した時点で通常どおり解決する。
関数の型 `∀ā. [制約]* → P → …`（trait のメソッドは `∀p̄. [C[p̄]] → ∀ā. [制約]* → P → …` のように量化と制約が交互に並ぶ）の `P` を受け手の型と見比べて（`matchType`。形を上から比べるだけで推論はしない）`P` に現れる型引数を決め、それが型引数の列の先頭に並んでいることを確かめる。残りの型引数は名前の直後に書かれたもの（`Select.typeArgs`）で、数が合わなければエラー。型引数を順に適用しながら間の制約を解決した関数を `Select.resolved` に記録し、SelectDesugar が受け手への適用に展開する。標準ライブラリの trait は受け手が最初の引数（`map(xs)(f)`・`foldLeft(xs)(start)(f)`・`traverse[a][g][b](xs)(f)`）で、Deriver の正準シグネチャもこの形。
パーサは、式の続きの `.f[T]` を `Select` に、`.` を含む名前 `a.b.c[T]` を `Path` に読む。`Path` は型検査が「値を指すいちばん長い前半」を値として読み、残りを `Select` にする（`checkPath`。全体が修飾された名前として読めるならそれが優先される。前半のどれも値でなければ、全体を修飾された名前として扱う）。型検査後の AST に `Path` は残らない。

### 名前付きフィールド

構成子が 1 つの data は、構成子の引数に名前を付けられる（`MkStudent(name: String)(math: i32)`。`DataConstructor.fieldNames`）。名前付きフィールドは、型の module のメンバーとしてアクセサ `Student.name: ∀ā. Student[ā] → フィールドの型` を持つ。型検査（`checkTopData`）がアクセサの名前と型を Env に登録し（`defineAccessors`。module を閉じるときの読み直しでも同じ関数を使う）、型の module を作る（または足す）。`s.name` はドット記法の規則で `Student.name(s)` になるので、レコード専用の解決はない。アクセサの本体は RecordDesugar（ModuleDesugar の直後）が `Λā. λr. match r { C(x̄) => xᵢ }` として宣言の後ろに生成する。パターンは位置のまま。
すべてのフィールドに名前がある構成子は、名前でも構築できる（`MkStudent(name = "a", math = 1)`。`NamedApp(constructor, arguments, positions)`。パーサは式の続きの `(x = e, …)` を読む）。型検査（`checkNamedApp`）は、名前の前が構成子に型引数を渡し終えたもの（`ConstructorDef.fieldNames`）であることと、すべてのフィールドが 1 回ずつ渡されていることを確かめ、各引数のフィールドの位置を記録する（エラーは `NamedArgumentsInvalid`）。RecordDesugar が位置の適用に直す（順番がフィールドの順と違えば、書いた順に評価するよう let で先に束縛する）。構成子が複数の data・式の中の data・opaque data には書けない（`FieldNamesNotAllowed`）。

### match のパターン

`match` は `PatternMatch(scrutinee, cases)` としてパースされ、パターン（`Pattern`: `_`・変数・構成子（入れ子可）・リテラル）を持つ。引数のない名前は、型検査がスコープ中の構成子でなければ変数の束縛（`Bind`）と決める。型検査はノードを保ち、構成子パターンにそのデータ型の全構成子と型引数適用済みのフィールド型（`siblings`）、リテラルに解決済みの `==` を記録する。
PatternDesugar は行列から決定木を作り、構成子 1 段の `Match`（ChurchEncoder が符号化する形）と `if` / `let` に展開する。複数の葉から使われる本体は関数として一度だけ置く。網羅性と到達不能な分岐は、型検査が同じ決定木で調べる（`PatternDesugar.check`）。選択肢 `p | q`（`Pattern.Or`）は脱糖の入口で選択肢ごとの行に展開し（同じ分岐を指すので本体は共有される）、条件付きの分岐（`PatternCase.guard`）は「条件が偽なら後続の行へ進む」節点になり、網羅には数えない。リテラルは `==` に宣言された関数で比べる（`Pattern.Literal` が解決済みの関数を持つ。bool だけは分岐で調べる）。束縛でのパターン（`LetPattern` ノード、context の `ContextStep.BindPattern` / `LetPattern`）は分岐が 1 つの match で、すべての値に当たるものに限る（`checkBindingPattern`）。context の束縛は ContextDesugar が match にするので、PatternDesugar はその後に走る（`Compiler.desugar`）。`fold` の分岐は従来どおり構成子 1 段と束縛子だけ（`MatchCase`）。

### 演算子（infixl / infixr / infix / prefix 宣言）

演算子と関数の対応は言語に埋め込まず、ライブラリが `infixl 6 + = add` / `prefix - = neg` と宣言する（`TopOperator(op, fixity, function)`。`fixity` が None なら前置）。右辺は `∀a. [制約]* → a → …` の形の多相関数なら何でもよく（`operatorShape`）、`a ⊕ b` は「宣言された関数に左の値の型を型引数として渡したもの」の適用として検査する（`operatorFunction`: 型適用の時点で制約が解決されるので、trait のメソッドならインスタンス解決になる）。
記号は記号文字の並びの最長一致で、決まった集合はない（`ParserAST.operatorSymbol`。`=` `=>` `->` `:` は予約）。優先順位（0〜9）と結合性も宣言の一部でスコープに従うので、パーサは式を並びのまま `OpSeq(first, rest: Seq[OpStep])` として読み、型検査がスコープ中の宣言で `BinOp` の木に組む（`groupOperators`。同じ優先順位は両方 infixl か両方 infixr のときだけ並べられる）。前置は二項より強い（`UnaryOp`）。型検査は `BinOp` / `UnaryOp` ノードを残して解決済みの関数を `resolved` に記録し、OperatorDesugar が適用に展開する。型検査後の AST に `OpSeq` は残らない。
宣言は他の宣言と同じスコープに従う（`Names.operators: Map[String, OperatorDef]`。キーは二項なら記号、前置なら `Operators.prefixKey`。宣言より後で見え、module のメンバーになり、`open` / `private` に従い、後の宣言が前を隠す）。`&&` と `||` だけは bool 専用の組み込みで（`Operators.builtin`。優先順位 3 と 2、宣言不可）、`if` に脱糖する（`resolved = None`）。`std/operators` は演算子ごとに 1 つの trait（`Add` / `Sub` / … / `Eq` / `Ord`）を置き、組み込みの型のインスタンスを `intrinsic` で定義する。Generator は演算子ノードを受け取らない。

### 構文に宣言された関数（interpolate / literal）

演算子と同じく、文字列埋め込み・リテラル・組も、ライブラリが宣言した関数を呼ぶ（`TopSyntax(hook, functions)`、`SyntaxHook`。スコープは `Names.syntax` で演算子と同じ規則）:

- `interpolate = show`: `` `a{x}` `` の `{x}` は `show[T](x)`（String も例外なく通す）。`StrInterp(parts: Seq[InterpPart])`
- `literal [] = Nil, Cons`: `[x, y]` は `Cons(x)(Cons(y)(Nil))`（右から組み立てる。空の `[]` はエラー `EmptyListLiteral`）。`ListLit(elements, nil, cons)`
- `literal setOf = Set.empty, Set.cons`: 名前付きのリテラル `setOf[x, y]`。名前は値と同じ名前の空間にあり（`Env.withValue` / `exportValue` が同名のリテラルを隠す）、module のメンバーになる（`namedLiteral`）
- `literal (,) = Pair` / `literal (,,) = Triple`: 組 `(x, y)` は `Pair(x)(y)`。`TupleLit(elements, resolved)`。組の型 `(A, B)` は、型検査（tcAlg の `TupleType`）が宣言した関数の結果の型に読み替える（`tupleType`）

型引数は値の型から読む（`instantiateFromValues`。関数の引数の型を値の型と `matchType` で見比べる。ドット記法と同じ照合）。宣言できる関数の形は `syntaxShape` が確かめる（どの型パラメータも、値を受け取る引数の型に現れる。リテラルは `nil: ∀ā. … → R` と `cons: ∀ā. … → E → R → R`）。型検査は具体化した関数をノードに記録し、OperatorDesugar が適用に展開する。
`name[…]` は型引数とも要素とも読めるので、パーサは名前の直後の `[ … ]` を `Path` に読み、型として読めるものは式としての読み（`Path.literal`。`typeAsExpr`）も持つ。型検査は `name` が名前付きのリテラルなら要素、そうでなければ型引数として検査する（使わない方の子は検査しない）。`Path.nameSpan` は名前の部分の位置（エラーが型引数を受け取った値を指せるように）。
パターンの `[p, q]` / `(p, q)`（`Pattern.Syntax`）は、宣言した関数が構成子なら、型検査がその構成子のパターンに読み替える（`[p, q]` は `Cons(p)(Cons(q)(Nil))`。構成子でなければ `SyntaxPatternNotConstructor`）。`interpolate` と `literal` は式のキーワード（宣言の前の行の式の続きと読まないため）。

### エラー表示（Diagnostic.scala）

エラーの中身（`CompileError` の各 case）と表示は分かれている。`CompileError.render` は 1 行の要約（テストが文字列で確かめる）で、利用者に見せる表示は `Diagnostic.render` が作る: 見出し・主な位置の下線と説明・関係する位置・note・help。文言は `Diagnostic.describe`（case ごと）と `syntaxError`（構文エラー）にある。

- 位置は `Span(start, end, file)`。パーサが各ノードに範囲を付け（`IndexedRec` の注釈）、import されたファイルのノードにはそのファイルを記録する
- 型検査の規則は親のノードで走るが、エラーは原因の子で報告する。型付きノードと元の範囲の対応を `Env.spans`（`SpanTable`、同一性で引く）に記録し、`failAt` / `expectAt` / `guardAt` が子の位置を引く。注釈つきの束縛は `blame` が λ・Λ の中へ進み、本体と結果の型の注釈を指す
- エラーに付ける情報は包む case で表す（どれも `render` と `core` は中身のまま）: `At`（主な位置）、`RelatedTo`（関係する位置と理由 `Related`）、`Hinted`（Env から得た手がかり `Hint`: 似た名前・宣言済みのインスタンス・`rec` の案内など。エラーが起きたノードの Env で一度だけ付ける。`TAnalyser.enrich`）、`RequiredBy`（どのインスタンスの文脈として必要になったか）
- 宣言の検査に失敗しても後続の宣言の検査を続け（`TAnalyser.checkAll`。top-level と module の中）、エラーを `CompileError.Multiple` にまとめる。失敗した宣言が定義するはずだった名前（`Env.failed` / `failedTraits`）を使ったことによるエラーは報告しない（`causedByFailure`）
- 1 つの宣言の中でも、互いに独立な部分（impl の各メソッド、rec の組の各メンバー、trait の各デフォルト）は `checkEach` が全部検査してエラーをまとめる。式の中は最初のエラーで止まる
- パターンは部分パターンごとに位置を持つ（`Pattern.Located`。型検査が外す）
- 表示を変えたら `UPDATE_DIAGNOSTICS=1 sbt "testOnly *DiagnosticSuite"` で `src/test/diagnostics/*.txt` を書き直し、差分を確認する

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

- `stdlib/*.lam` — `import std/...` で解決される標準ライブラリ。`StandardLibrary.scala` が `intrinsic[add_i32]` などの名前を組み込み演算へ対応付ける。型ごとの関数は、その型の module に、値を最初の引数に取る形で置く（ドット記法で呼べるように）: `List` / `Option` / `Pair` / `char` / `String` / `i32` / `Result` / `Map` / `Set`（Map は AVL 木で、構成子 `MapEmpty` / `MapNode` は直接使わない）。trait のメソッドと同じ名前の関数は module に置かない（ドット記法が曖昧になる）
- `runtime/ffi.c` — `foreign` で参照する C 関数の実装。関数値はコードポインタ + 環境ポインタのクロージャ
- Generator: 束縛はすべてヒープの環境フレームに置き、`[x29, -16]` が現在のフレームを指す。関数本体の末尾位置の呼び出し（`Env.tail`）はフレームを畳んでジャンプにするので、末尾再帰はスタックを消費しない。メモリは `malloc` するだけで解放しない
- `src/test/lam/*.lam` — EndToEndSuite が実行するプログラム（期待する出力はスイート側に書く）
- パッケージは `com.yuuki14202028`（sbt の `idePackagePrefix`）。ソースは `src/main/scala/` 直下に置き、パッケージ宣言のみ行う

## 規約

- 言語の説明は `docs/language.md`。`lam` のコード例は DocSuite がコンパイルして確かめるので、構文や標準ライブラリを変えたら文書も直す

- ドキュメント・コミットメッセージは日本語。コミットは gitmoji prefix（`:sparkles:` 等）を使う