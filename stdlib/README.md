# 標準ライブラリ (Standard Library)

このディレクトリには、Lambda言語の標準ライブラリが含まれています。

## モジュール一覧

### 基本データ型

- **option.lam** - `Option[a]` 型（`Some(a)` または `None`）と module `Option`（`o.getOrElse(d)` / `o.isSome` など）
- **either.lam** - `Either[a][b]` 型（`Left(a)` または `Right(b)`）
- **pair.lam** - `(a, b)` 型（2つの値の組。`p.first` / `p.second` / `p.swap`）
- **list.lam** - `List[a]` 型（連結リスト）と module `List`（`xs.length` / `xs.filter(p)` / `xs.sorted` / `xs.head` / `xs.concat(ys)` など。どの関数もリストを最初の引数に取る）
- **id.lam** - `Id[a]` 型（恒等ファンクター、`Functor` は derive）
- **result.lam** - `Result[e][a]` 型（`Ok(a)` または `Err(e)`）と module `Result`（`r.getOrElse(d)` / `r.mapErr[f](g)` / `r.toOption` など）
- **map.lam** - `Map[k][v]` 型（キーの順で並べた AVL 木）と module `Map`（`m.insert(k)(v)` / `m.lookup(k)` / `m.remove(k)` / `m.toList` など）
- **set.lam** - `Set[a]` 型と module `Set`（`s.insert(x)` / `s.contains(x)` / `s.union(t)` など）

### 型クラス (Traits)

メソッドはどれも受け手を最初の引数に取るので、ドット記法で呼べる（`xs.map[b](f)`、`xs.foldLeft[b](start)(f)`、`o.flatMap[b](f)`、`xs.traverse[Option][b](f)`、`n.show`）。型の module の関数と同じ名前にはしない（曖昧になる）。

- **functor.lam** - `Functor` trait（`map` 操作）
- **applicative.lam** - `Applicative` trait（`pure`, `apply` 操作）
- **monad.lam** - `Monad` trait（`flatMap` 操作、値が第1引数）
- **foldable.lam** - `Foldable` trait（`foldLeft`, `foldRight` 操作）
- **eq.lam** - `Eq` trait（等値比較）
- **ord.lam** - `Ord` trait（順序比較、`Eq` のサブクラス）
- **semigroup.lam** - `Semigroup` trait（結合的な二項演算）
- **monoid.lam** - `Monoid` trait（`Semigroup` + 単位元）

### 関数ユーティリティ

- **function.lam** - 基本的な関数ユーティリティ
  - `id[a](x: a): a` - 恒等関数
  - `const[a][b](_x: a)(y: b): b` - 常に2番目の引数を返す
  - `compose[a][b][c](f: b → c)(g: a → b): a → c` - 関数合成
  - `flip[a][b][c](f: a → b → c): b → a → c` - 引数の順序を入れ替える
  - `curry[a][b][c](f: (a, b) → c): a → b → c` - カリー化
  - `uncurry[a][b][c](f: a → b → c): (a, b) → c` - アンカリー化

### インスタンス定義

`Functor` / `Foldable` / `Traversable` と、構造どおりの `Eq` / `Ord` / `Show` のインスタンスは `derive` による自動導出です。

- **option_instances.lam** - `Option` の `Functor` / `Foldable` / `Traversable` / `Eq`（derive）, `Applicative`, `Monad`, `Semigroup`, `Monoid`, `Ord`（None が先）
- **either_instances.lam** - `Either` の `Functor` / `Foldable` / `Traversable` / `Eq` / `Ord`（derive）, `Applicative`
- **pair_instances.lam** - `Pair` の `Functor` / `Foldable` / `Traversable` / `Eq` / `Ord`（derive）
- **result_instances.lam** - `Result` の `Functor` / `Foldable` / `Traversable` / `Eq` / `Show`（derive）, `Applicative`, `Monad`
- **list_instances.lam** - `List` の `Functor` / `Foldable` / `Traversable` / `Eq` / `Ord`（derive）, `Applicative`, `Monad`, `Semigroup`, `Monoid`
- **eq_instances.lam** - 基本型（`i8`, `i16`, ..., `bool`, `unit` など）の `Eq` インスタンス
- **ord_instances.lam** - 数値型の `Ord` インスタンス
- **semigroup_instances.lam** - 基本型の `Semigroup` インスタンス
- **monoid_instances.lam** - 基本型の `Monoid` インスタンス

### I/O

- **io.lam** - `IO[a]` 型と基本的な I/O 操作（実験的、prelude には含まれない）

### 演算子

- **operators.lam** - 演算子ごとの trait と演算子の宣言（`infixl 6 + = add`）、すべての数値型・`bool`・`char` のインスタンス
  - 算術演算: `Add`（`+`）, `Sub`（`-`）, `Mul`（`*`）, `Div`（`/`）, `Mod`（`%`）, `Neg`（前置 `-`）
  - 比較演算: `Eq`（`==` `!=`）, `Ord`（`<` `<=` `>` `>=`）
  - 論理・ビット演算: `Not`（前置 `!`）, `BitAnd`（`&`）, `BitOr`（`|`）, `BitXor`（`^`）
  - 自分の型で演算子を使うには、対応する trait を実装する（`impl Add[Money] { … }`）
  - 論理演算: `and`, `or`, `not`, `short_and`, `short_or`

### Prelude

- **prelude.lam** - よく使用されるモジュールを一括してインポート

## 使用例

`stdlib_test.lam`（リポジトリ直下）が全体の使用例を兼ねています。

### derive を使用する

```lambda
import "std/functor"
import "std/foldable"

data rec Tree[a] = { Leaf, Node(a)(Tree[a])(Tree[a]) }

derive Functor[Tree]
derive Foldable[Tree]
```

### Option を使用する

```lambda
import "std/prelude"

let increment(x: Option[i32]): Option[i32] =
  x.map[i32](λn: i32. n + 1)

let divSafe(x: i32)(y: i32): Option[i32] =
  if y == 0 then None[i32] else Some[i32](x / y)
```

### List を使用する

```lambda
import "std/prelude"

let nums(): List[i32] = [1, 2, 3]

let total(): i32 =
  nums().foldLeft[i32](0)(λacc: i32. λx: i32. acc + x)
```

### Context 記法（do 記法）

```lambda
import "std/prelude"

let example(): Option[i32] = context[Option] {
  x: i32 = Some[i32](10);
  y: i32 = Some[i32](20);
  x + y
}
```
