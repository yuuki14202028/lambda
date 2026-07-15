# CoC 拡張設計 — 階層表層 + 統一カーネル方式

λω（System Fω 相当）である本言語を CoC（Calculus of Constructions）へインクリメンタルに拡張するための設計。
表層 AST は現在の Expr / Type / Decl の階層構造を**維持したまま**、意味論（正規化・定義的等価性・最終検証）だけを
小さな統一カーネルに集約する。以後の言語進化は表層への**ノード追加**とカーネルへの**フック追加**で行い、
全面改修は二度と行わない。

## 方針

- **Sort は古典的 CoC の 2 つ**（`*` と `□`）。宇宙階層・Prop/Type 分離は導入しない（`□` に型を与えない限り必要にならない）
- **帰納型はカーネルに入れない**（当面）。`data` / `match` / `fold` は不透明な定数としてカーネルに写す。
  将来の帰納型導入が「追加」で済むよう、拡張点をあらかじめ確保する（後述）
- **表層 AST（AST.scala / ParserAST）は統合しない**。依存型は追加ノード 2 つで表現する
- 統一（項・型・カインドの一本化）は**カーネル内部にのみ**存在する。表層の `→` / `∀` / `Λ` / カインドは
  Lower がすべてカーネルの Π / λ / Sort に潰す

### 2 案の比較（決定の記録）

| | 案 A: 表層も Π に統一 | 案 B: 階層維持 + ノード追加（採用） |
|---|---|---|
| AST.scala / ParserAST | Expr / Type 統合の大改修 | 既存ノード無変更、case 追加のみ |
| 移行 | Big Bang 一回 | 各ステップ独立・常にテスト通過 |
| 冗長性 | なし | Arrow / ForAll / Pi が並存（Lower が吸収） |

案 B でもカーネルは完全に Π 統一された CoC であり、意味論上の妥協はない。
意味論がカーネルに集約されるため、将来「表層も Π に一本化」する場合は意味を変えないリファクタリングとして実施できる
（案 B → 案 A は安全、逆は不可能という非対称性がある）。

## 全体アーキテクチャ

```
ParserAST → ImportResolver
  → TAnalyser        型検査。表層 AST 上・糖衣ノード保持のまま（従来通り）
                     依存型の等価性判定のみカーネルの conv を呼ぶ
  → ContextDesugar   従来通り（型付き表層 AST → 型付き表層 AST）
  → TraitEncoder     従来通り
  → Lower            最後の脱糖: 型付き表層 AST → カーネル Term
  → カーネル再検査   全脱糖フェーズの型保存を検証（失敗 = コンパイラのバグ）
  → ChurchEncoder → Generator   従来通り（カーネルは実行時表現を知らない）
```

### 責務分担

| コンポーネント | 責務 |
|---|---|
| TAnalyser | どのノードで・何と何を比べるか・エラー報告（ユーザー起因エラーはすべてここ） |
| カーネル | 2 つの型が定義的に等しいかの Yes/No（βδ 正規化と比較）、および最終再検査 |
| Lower | 表層 → カーネルの機械的写像。検査もエラー報告もしない |

### エラー 3 層設計との整合

既存の規約（構造化 `CompileError` / `invariant`）にそのまま乗る:

- ユーザー起因エラー → TAnalyser が表層ノードと offset から `CompileError` を構築（従来通り、Caret 付き）
- カーネル再検査の失敗 → 前段フェーズ（脱糖含む）のバグなので `invariant`（`sys.error`）で即落とす
- カーネルは Coq のカーネル同様「独立した検証器」として働き、「型検査後に脱糖する」方針の安全網になる
  （脱糖が型を壊していないことを毎コンパイル機械的に検証する）

## カーネル

新規ファイル `Kernel.scala`（または `kernel/` 以下に分割）。既存の GADT + HFunctor 機構は**使わない**。
カーネルを通るフェーズは検査 1 つだけであり、高階再帰スキームの重さが割に合わないため、素朴な enum で書く。

### Term（凍結対象）

```scala
enum Sort { case Star, Box }

// 束縛は de Bruijn index。name は表示専用
enum Term {
  case Vr(ix: Int)
  case Srt(s: Sort)
  case Pi(name: String, dom: Term, cod: Term)
  case Lam(name: String, dom: Term, body: Term)
  case App(f: Term, a: Term)
  case Let(name: String, ty: Term, value: Term, body: Term)
  case Const(name: String)                  // グローバル定数への参照
  case Lit(value: String, tpe: String)      // 数値・文字・bool・文字列・unit リテラル
}
```

**この enum は帰納型導入後も変更しない。** `match` / `fold` すらカーネル項にはならず、
将来の拡張はすべて `Const` の解釈側（GlobalDecl と NbE のフック）に吸収される（Lean のカーネルと同じ方式）。

de Bruijn 採用により、`substType` にある捕獲回避リネーム（`rebind` / `freshTypeVariable`）はカーネルには存在しない。

### PTS としての型付け

判断は `Γ ⊢ t : T` の 1 つ。公理は `* : □`（`□` の型を問うたら invariant エラー）。
Π 形成規則は Rule 集合で制御する:

```scala
val rules: Set[(Sort, Sort)] = Set(
  (Star, Star),   // 項に依存する項（λ→）
  (Box,  Star),   // 型に依存する項（λ2）
  (Box,  Box),    // 型に依存する型（λω̲）
  (Star, Box),    // 項に依存する型 ← これを足すと CoC
)
```

カーネルは最初から 4 規則すべてを実装する（一般形で書く方が短い）。
表層でどこまで解禁するかは表層文法・TAnalyser 側で制御する。

### GlobalDecl — 拡張点 1

カーネルはグローバル定数表を持つ。宣言種別が将来の帰納型の差し込み口になる:

```scala
enum GlobalDecl {
  case Def(ty: Term, body: Term)   // δ 展開可能（透明）
  case Axiom(ty: Term)             // 型のみ。展開されない（不透明）
  // 将来: case Inductive(...)     // 追加はここ。Term には触れない
}

case class GlobalEnv(decls: Map[String, GlobalDecl])
```

表層要素の写し方:

| 表層 | カーネルでの姿 | 簡約 |
|---|---|---|
| `let` / トップレベル定義 | `Def` | δ で展開 |
| `letRec` | `fix` 公理経由（下記） | なし |
| `foreign` / intrinsic（`__add_i32` 等） | `Axiom` | なし |
| `data T` の型構成子 | `Axiom`（例: `Option : Π(_: *). *`） | なし |
| コンストラクタ | `Axiom`（例: `Some : Π(a: *). Π(_: a). Option a`） | なし |
| `match` / `fold` | 除去子 `Axiom` の適用（下記） | なし |
| リテラル | `Lit` | なし |

**除去子**: `data` 宣言ごとに非依存の除去子定数を自動生成し、`match` / `fold` はその適用に Lower する。

```
$match_Option : Π(a: *). Π(r: *). Π(_: Π(_: a). r). Π(_: r). Π(_: Option a). r
$fold_Nat     : Π(r: *). Π(_: r). Π(_: Π(_: r). r). Π(_: Nat). r
```

網羅性検査などは従来通り TAnalyser の責務。カーネルからは「除去子という名の関数適用」しか見えない。

**再帰**: `letRec` は次の公理で表す:

```
$fix : Π(A: *). Π(_: Π(_: A). A). A
```

`letRec x: T = e in b` → `Let(x, T, App(App($fix, T), Lam(x, T, e)), b)`。
`$fix` は `Axiom` なので中立項として簡約が止まり、停止性の問題（型検査の決定可能性）をカーネルから隔離する。

> **健全性の注記**: `$fix` の型はあらゆる型に住人を与えるため、論理として読むと矛盾する。
> 現時点では証明を書く手段（帰納型）がないため実害はない。帰納型導入時に
> 「証明の文脈では `$fix` / `foreign` 由来の公理を禁止する」検査（または停止性検査）を表層側に足す。

### NbE — 正規化と conv、拡張点 2

conversion（定義的等価性の判定）は NbE（Normalization by Evaluation）で実装する。
証明・型レベル計算は実質「型検査中のプログラム実行」であり、逐次置換ベースでは性能が持たないため。

```scala
enum Value {
  case VSort(s: Sort)
  case VPi(name: String, dom: Value, cod: Closure)
  case VLam(name: String, dom: Value, body: Closure)
  case VNeutral(head: Head, spine: List[Value])   // 簡約が止まった項
  case VLit(value: String, tpe: String)
}
enum Head { case HVar(lvl: Int); case HConst(name: String) }

case class Closure(env: List[Value], body: Term)  // apply(v) = eval(v :: env, body)
```

- `eval: (GlobalEnv, List[Value], Term) => Value` — β（λ 適用）と δ（`Def` の展開）。
  `Const` に出会ったときの分岐が**唯一の簡約フック**であり、将来の ι（除去子がコンストラクタに当たったら計算）は
  この分岐に 1 ケース足すだけで入る
- `quote: (Int, Value) => Term` — de Bruijn level から index への読み戻し
- `conv: (Int, Value, Value) => Boolean` — Value 上の構造比較。関数の η は**有効**にする
  （片側が VLam なら両辺に fresh 変数を適用して比較。実装が軽く、等価性が使いやすくなる）

### カーネル型検査（再検査）

Lower の出力は全束縛に注釈が付いた Church スタイルなので、推論主体の単純な検査で足りる:

```scala
def infer(genv: GlobalEnv, ctx: List[Value], t: Term): Value   // 型を Value で返す
```

- `App(f, a)`: `infer(f)` が `VPi` であることを確認し、`a` を検査して `conv` で突き合わせ、閉包に `a` を適用
- `Pi(x, A, B)`: `A : s₁`、`B : s₂`（`x: A` を文脈に足して）、`(s₁, s₂) ∈ rules` を確認
- 失敗はすべて `sys.error`（ここに来る項は検査済みのはずであり、失敗はコンパイラのバグ）

## Lower — 表層 → カーネルの写像

新規ファイル `Lower.scala`。パイプライン末尾（TraitEncoder 後）で全宣言を写し、カーネル再検査に渡す。
`TopTrait` / `TopImpl` / `Context` は TraitEncoder / ContextDesugar で消えているため対応不要。

| 表層ノード | カーネル項 |
|---|---|
| `Abs(x, T, e)` | `Lam(x, L(T), L(e))` |
| `TyAbs(A, k, e)` | `Lam(A, LK(k), L(e))` |
| `App` / `TyApp` | `App` |
| `Arrow(A, B)` | `Pi("_", L(A), L(B))` |
| `ForAll(A, k, T)` | `Pi(A, LK(k), L(T))` |
| `Pi(x, A, B)`（新設） | `Pi(x, L(A), L(B))` |
| `ExprInType(e)`（新設） | `L(e)` |
| `TypeAbs` / `TypeApp` | `Lam` / `App` |
| `Var` / `TypeVar` | `Vr`（束縛変数）/ `Const`（トップレベル） |
| `Primitive(n)` | `Const(n)` |
| `Let` | `Let` |
| `LetRec` | `$fix` 適用の `Let` |
| `Match` / `Fold` | 除去子 `Const` の適用 |
| `Num` / `Char` / `Bool` / `StringLit` / `UnitLit` | `Lit` |
| `BinOp` / `UnaryOp` / `Intrinsic` | 演算子定数（`__add_i32` 等）の適用 |
| `If` | `$ite : Π(A: *). Π(_: bool). Π(_: A). Π(_: A). A` の適用 |
| `Block` | `Let` の連鎖（捨て値は `"_"` 束縛） |
| `StrInterp` | 連結プリミティブ定数の適用 |
| `Foreign` | `Axiom` 参照 |

カインドの写像 `LK`:

| Kind | カーネル項 |
|---|---|
| `Star` | `Srt(Star)` |
| `Arrow(k₁, k₂)` | `Pi("_", LK(k₁), LK(k₂))` |
| `KPi(x, T, k)`（新設、後述） | `Pi(x, L(T), LK(k))` |

表層では項と型の名前空間が分かれているが、de Bruijn 化するので衝突は問題にならない。
Lower は文脈（名前 → index の対応）を持ち回る純粋な写像で、Either 層を持たない
（失敗 = 検査済み AST の破損 = invariant）。

### TAnalyser からの conv 呼び出し

依存型解禁後、TAnalyser の等価性判定（現在の `Equivalence.alpha` / `normalize` の呼び出し箇所）は
「両辺を Lower して `conv` を呼ぶ」に差し替える。このとき:

- **文脈付き Lower**: 比較対象は自由変数を含むため、TAnalyser の `Env` からカーネル文脈を作って渡す
- **KernelEnv の逐次構築**: δ 展開には検査済みトップレベル定義のカーネル項が要る。
  TAnalyser の宣言単位 StateT に `GlobalEnv` を足し、各宣言の検査完了後に Lower して登録する
- **糖衣ノードの扱い**: 型の中の項に未脱糖の糖衣（`context` ブロック等）が含まれる場合、
  conv 時 Lower はそれを**不透明な中立項**として扱う（構造が同一のときだけ等しい）。
  健全性は保たれ、失われるのは糖衣を含む項の定義的等価性のみ。実用上型の中に置かれる項は小さい式であり、
  必要になった時点で ContextDesugar / TraitEncoder の変換をノード単位で再利用できるよう括り出す

## 表層の変更 — 段階的導入計画

既存ノードの変更ゼロ・削除ゼロ。`Arrow` / `ForAll` / `TyAbs` / `Kind` はすべて残し、Lower が Π に潰す別名になる。

### Step 0 — カーネル導入（表現力の変化なし）

- 新規: `Kernel.scala`（Term / NbE / infer / GlobalEnv）、`Lower.scala`
- パイプライン末尾に Lower + 再検査を接続（結果は捨てる。コード生成への影響ゼロ）
- AST.scala / ParserAST / TAnalyser は無変更。**既存テストがそのまま全部通る**
- テスト: KernelSuite（NbE 正規化・conv・PTS 規則の単体テスト）、
  LowerSuite（stdlib 全ファイルと既存サンプルが再検査を通ることの確認）

### Step 1 — 依存関数型の文法

- AST.scala: `Pi(variable: Variable, from: R[Type], to: R[Type]) extends AST[R, Type]` を追加
  （`to` の中で項変数 `variable` を使える、が唯一の新規性）
- ParserAST: `(x: A) → B` の規則を追加。`B` が `x` を使わなければ従来の `A → B` と同義
- KAnalyser / TAnalyser / ShowAST / Lower に各 1 case
- 注: `ExprInType` がない間、`B` は `x` に言及できないため Step 1 単体では `Arrow` の別記法にすぎない。
  Step 2 とセットで意味を持つ（マージは分けてよい）

### Step 2 — 型の中の項（依存型の実質解禁）

- AST.scala: `ExprInType(expr: R[Expr]) extends AST[R, Type]` を追加。
  `Vec i32 (plus x y)` の `plus x y` はこのノードで型に埋まる
- **Kind の依存化**: `Nat → *` のようなカインド（型族の分類）が必要になるため、
  `Kind` を `Kind[R[_]]` にパラメータ化し `KPi(variable: Variable, dom: R[Type], cod: Kind[R])` を追加する。
  既存 2 コンストラクタは無変更だが、型パラメータ追加が Kind を使う全箇所へ機械的に波及する
  （**本計画で唯一、既存定義のシグネチャが変わる箇所**。hmap の対応も必要）
- `substExprInType` を追加: 適用 `f a`（`f : (x: A) → B`）の結果型 `B[x := a]` の構築に使う。
  `substType` の相似形で、`ExprInType` 内の `Var(x)` を置換する（Pi 束縛の捕獲回避を含む）
- TAnalyser: 適用規則に Pi ケースを追加。`ExprInType` ノードでは Pi の定義域から期待型が分かるので、
  埋め込まれた式を通常の式検査に回す（型の中の位置で普通の型エラーが出る）
- 等価性判定をカーネル conv に差し替え（上述）。`Equivalence.scala` は役目を終える

### Step 3 — 帰納型（将来。本設計のスコープ外だが差し込み口を明記）

変更は 3 点に閉じ、Term・検査骨格・conv アルゴリズム・バックエンドは不変:

1. `GlobalDecl.Inductive` を追加し、`data` 宣言をこちらで登録
2. 除去子定数の型を依存版に強める: `Π(r: *). …` が `Π(P: Π(_: Nat). *). …` になる
   （定数の「枠」は同じで、入る型が変わるだけ）。表層は `match` / `fold` に motive のオプションフィールドを追加
3. NbE の `Const` 分岐に ι を足す: 除去子がコンストラクタに当たったら計算する

このとき Church encoding では帰納法原理が導出できない（Geuvers の定理）ため、
定理証明（例: `plus_comm : Π(x y: Nat). Eq Nat (plus x y) (plus y x)`）にはこの Step 3 が必須になる。
併せて証明消去（Erasure）と `$fix` の隔離（停止性検査）を導入する。

## エラー報告

依存型で新しく出るエラーは 3 種類で、いずれも報告主体は TAnalyser（位置精度・Caret は従来通り）:

1. **計算しないと分からない不一致**（`Vec i32 4` vs `Vec i32 (plus 1 2)`）:
   conv が No を返し、TAnalyser が報告。`CompileError` に「書かれたままの表層型」に加えて
   「カーネルが返す正規形」をデータで持たせ、render で `（計算後: Vec i32 3）` のように併記する。
   正規形の表示にはカーネル Term の印字関数（ShowAST のカーネル版）を用意する
2. **型の中の項が型エラー**（`Vec i32 true`）: `ExprInType` ノードで通常の式検査が走るだけ。新機構不要
3. **開いた項どうしの不一致**（`plus x y` vs `plus y x`）: 正規形（中立項）の構造比較で No。
   定義的等価でないものの同一視には証明が要る、という CoC の意味論通りの挙動

## 未決事項

- conv の η を関数以外（unit の η など）に広げるか — 当面は関数のみ
- `Lit` の型レベル計算（`1 + 2` を δ フックで畳む）— 帰納型より先に欲しくなったら追加。追加であって改修ではない
- カーネル Term の印字における de Bruijn → 名前の復元規則（表示専用 name の衝突処理）
- conv 時 Lower の不透明中立項のキー設計（ノード同一性ベースか、構造ハッシュか）