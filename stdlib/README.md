# 標準ライブラリ (Standard Library)

このディレクトリには、Lambda言語の標準ライブラリが含まれています。

## モジュール一覧

### 基本データ型

- **option.lam** - `Option[a]` 型（`Some(a)` または `None`）
- **either.lam** - `Either[a][b]` 型（`Left(a)` または `Right(b)`）
- **pair.lam** - `Pair[a][b]` 型（2つの値の組）
- **list.lam** - `List[a]` 型（連結リスト）と `appendList` / `length` / `reverse` / `filter` / `head` / `tail`
- **id.lam** - `Id[a]` 型（恒等ファンクター、`Functor` は derive）

### 型クラス (Traits)

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
  - `curry[a][b][c](f: Pair[a][b] → c): a → b → c` - カリー化
  - `uncurry[a][b][c](f: a → b → c): Pair[a][b] → c` - アンカリー化

### インスタンス定義

`Functor` / `Foldable` のインスタンスは `derive` による自動導出です。

- **option_instances.lam** - `Option` の `Functor`（derive）, `Foldable`（derive）, `Applicative`, `Monad`, `Semigroup`, `Monoid`
- **either_instances.lam** - `Either` の `Functor`（derive）, `Applicative`
- **pair_instances.lam** - `Pair` の `Functor`（derive）, `Foldable`（derive）
- **list_instances.lam** - `List` の `Functor`（derive）, `Foldable`（derive）, `Semigroup`, `Monoid`
- **eq_instances.lam** - 基本型（`i8`, `i16`, ..., `bool`, `unit` など）の `Eq` インスタンス
- **ord_instances.lam** - 数値型の `Ord` インスタンス
- **semigroup_instances.lam** - 基本型の `Semigroup` インスタンス
- **monoid_instances.lam** - 基本型の `Monoid` インスタンス

### I/O

- **io.lam** - `IO[a]` 型と基本的な I/O 操作（実験的、prelude には含まれない）

### 演算子

- **operators.lam** - すべての数値型、`bool`、`char` に対する演算子
  - 算術演算: `add`, `sub`, `mul`, `div`, `mod`, `neg`
  - 比較演算: `eq`, `neq`, `lt`, `leq`, `gt`, `geq`
  - ビット演算: `and`, `or`, `xor`
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
  map[Option][i32][i32](λn: i32. __add_i32(n)(1))(x)

let divSafe(x: i32)(y: i32): Option[i32] =
  if __eq_i32(y)(0) then None[i32] else Some[i32](__div_i32(x)(y))
```

### List を使用する

```lambda
import "std/prelude"

let nums(): List[i32] = Cons[i32](1)(Cons[i32](2)(Cons[i32](3)(Nil[i32])))

let total(): i32 =
  foldLeft[List][i32][i32](λacc: i32. λx: i32. __add_i32(acc)(x))(0)(nums())
```

### Context 記法（do 記法）

```lambda
import "std/prelude"

let example(): Option[i32] = context[Option] {
  x: i32 = Some[i32](10);
  y: i32 = Some[i32](20);
  __add_i32(x)(y)
}
```
