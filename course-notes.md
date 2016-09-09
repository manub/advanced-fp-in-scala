`val f: (Foods => Response)`

`f` is a element of `(Foods => Response)` (all the functions from Foods to Response)

**Higher Order Functions**

a *Higher Order Function* is a function that *accepts* or *returns* a function.

e.g. 

```
class List[A] {
    // ...
    def map[B](f: A => B): List[B] = ... // higher order function
```

domain is *all the functions from `A` to `B`* - codomain is *all the `List[B]`*

Three parameters? *easy!*

`A, B => C` can be seen in two ways:

 - `(A, B) => C` where `(A, B)` is the cartesian product of `A` and `B`
 - `A => (B => C)`, a function from `A` to `B => C` - **curried form** 
 
**Polymorphic functions**

Scala doesn't support them!! We need to find a way around it...

`def square(x: Int): Int = x * x` is a *method* (because of the `def`) - it hasn't got a type!

`val square: Int => Int = x => x * x` *this* is a function!!! type is `Int => Int`

Function has a lot of useful methods (`compose`, `apply`, `andThen`)

`square` operates on a concrete type, so we call this a **monomorphical function**. 

Identity function: e.g. `"foo" => "foo"`

`val identity: A => A =  a => a`

but there's no way to declare a polymorphic method in scala!!! We need to use a trait instead and use the `apply` method to do the trick...

```
trait identity { def apply[A](a: A): A = a }

identity(1) = 1
```

et voila!!!


**Generic traits**

`trait` defines a **type!!!** - it's like introducing a new set (mathematical concept)

`trait List[A]` defines the `List` type

`case class Map[A]()`

`[A]` in this case is a *parameter list*. there are two parameter lists: *type* `[A, B, ...]` and *value* `(a, b, ...)`

`identity("foo")` -> in this case Scala is injecting the type parameter via type inference.

but there's no way to identify the type with an identifier... `def apply[A](a: A): A = a`: in this case `a` refers to a value. the type of a type would be just `*`! other languages allow you to identify types with identifiers (such as Haskell)

**POLYMORPHIC FUNCTIONS ARE VERY IMPORTANT!!!**

What's a *function combinator*? It's a **higher order function that takes 2 functions and returns a function**

Parser combinators are a classic example. See `functions.exercise3`



---

a **type** is a compile time description of a set of values

`String` describe the compile time set of *all possible strings*

**Sum** and **product** types. A language that has those types can define an **ADT** (*algebraic data types*)

A *product* data type is defined by the cartesian cross product of two types.

E.g. `(Int, String)` => all the possible combinations of `Int` and `String`

Let's have a `DayOfWeek` type. Cardinality is 7.
`MonthOfYear` has cardinality 12.

`(DayOfWeek, MonthOfYear)` has cardinality `7 * 12 = 84`

a Person has **both** a first name and a last name. We can use a product type to model that.

In Scala we can model a product type as a `case class`.

`case class Person(name: String, age: Int, ...)`

A *sum* data type is either one thing or another. `Either[String, Int]` is *either* a `String` or an `Int`. Can be geometrically modeled as two segments concatenated into one. The cardinality of the sum type is the sum of  the respective cardinalities.

`Unit` has cardinality 1: `()`

`Nothing` has cardinality 0.

The sum of a type `A` and `Nothing` is still `A`.

The product of a type `A` and `Unit` is still `A`.

Pre-dotty we can use inheritance to model sum types.

`sealed` allows inheritance only **inside** the file. by using this we can pattern match because the compiler will know if there are instances missing. `final` is so that we can't extend other days.

```
sealed trait DayOfWeek
final case object Monday extends DayOfWeek
...
final case object Sunday extends DayOfWeek
```

example: a **tree** can be seen as a composition of products and sums

```
sealed trait Tree[A]
final case class Leaf[A](v: A) extends Tree[A]
final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]
```

`Tree` is a **sum** (it can be a `Leaf` or a `Branch`). Both `Leaf` and `Branch` are **products**. `Tree` is a *sum of products*.

**Subtypes and supertypes**

`A` is a subtype of `B` if `A` is a subset of `B` (e.g. every `A` is a `B`)

`B` is a supertype of `B` if `B` is a superset of `A` (e.g. not every `B` is a `A`)

**Universally quantified types**

`sealed trait List[A]` - `List[A]` is a *universally quantified type* - it's quantified on `A`. `List[A]` is defining a category of types, not a concrete type itself. `List[A]` is a **type constructor** - compared to a *data constructor* which will give you instead a **instance of that type**.

`case class Person(name: String, age: Int)` -> `Person("Emanuele", 33)` that construct an instance of `Person`.

`sealed type Map[K, V]` is a type constructor that needs 2 types to instantiate a type.

**Types of types!!!**

`(v: Int)` means `v` is an element of the set of all `Int`. `[A: *]` means `A` is an element of the set of all types.

`*` is a kind of types. Hence we say **`*` is the kind of `A`**.

`Int`, `String`, `Boolean` are all of kind `*`. If you picture a set called `*` that contains all the types, `Int String Boolean` are all part of that set (it also contains `List[Int]`)

`List` has kind `* => *` - if I feed it a type, I get a type back.

`Either[A, B]` has kind `[*, *] => *` or `* => * => *`

Partially applied type constructor: `type Rez[A] = Either[Error, A]` (remember `Either` is right-biased!)

**Higher kinded types**

Function that accepts types and/or returns types.

`[(* => *), *] => *` is a valid kind. `Either` has kind `[*, *] => *`. The first kind can be seen as an `Either` in which the first type parameter is a type constructor. (e.g. `Either[List, Int]`)

Example in Scala: `case class Stack[F[_], A](left: F[A])` 

Type lambdas: `Stack[({type L[A] = Either[String, A]})#L, Int](Right(1))`

You can use type aliases - but not always (you need a scope to apply them to). There's a compiler plugin for this in this project :) `kind-projector`

**Existentials**

```
trait List[A]
def identity[A](v: A): A = v
```

Universal types - I choose the type. In an existential type, they choose the type. Not me (as in the previous snippet).

```
trait FileSystem {
    type File
    def ls: List[File]
    def touch(name: String): File 
}

class LinuxFileSystem extends FileSystem
 
val fs = LinuxFileSystem()

fs.ls: List[fs#File]
```

A user can provide their `File` type, but I have no idea what that type is! Any code that's written in `FileSystem` has to work with their `File`. My code is polymorphic in the type of `File`. 

I and they - who's that? Universal - they are the users of the function. Them is the callee - I'm the caller. The callee choose his type of `A`.

**Type classes**

`def sort[A](l: List[A]): List[A] = ???` how can I define this? This function is too polymorphic!

a `trait` is a type class. a Type Class means a type obeys to some laws.

`trait Debug[A]` is defined on type `A`.

```
trait Debug[A] {
    def show(a: A): String
    def read(s: String): Either[String, A] // reads a String and returns either an error or an A
}

// read(show(a)) == Right(a) 
// read(s) match {
//      case Left(_) => true
//      case Right(a) => show(a) == s
// }
```

you can use `ScalaCheck` to make sure your instances satisfy the type class law.

Code is in `typeclasses.scala` for this.

```
trait Debug[A] {
    def show(a: A) : String
    def read(s: String): Either[String, A]
  }

  object Debug {
    def apply[A](implicit v: Debug[A]): Debug[A] = v

    implicit val DebugInt: Debug[Int] = new Debug[Int] {
      def show(a: Int) = a.toString
      def read(s: String): Either[String, Int] = Try(s.toInt).map(Right(_)).getOrElse(Left("The value is not an integer"))
    }
  }

  Debug[Int].show(1)
//  implicitly[Debug[Int]].show(1)

  // use case for your own classes!

  case class Person(name: String, age: Int)

  object Person {
    implicit val DebugPerson: Debug[Person] = new Debug[Person] {
      override def show(a: Person): String = a.toString
      override def read(s: String): Either[String, Person] = ???
    }
  }
```

Always declare the instances of typeclasses in the right place! This is particularly true when you don't have control of the class.

Avoid implicit hell...

**Extension Methods** 

use `implicit class` instead.

```
implicit class DebugShowSyntax[A](self: A)(implicit ev: Debug[A]) {
    def show: String = ev.show(self)
}
```

This way you can simply write `1.show` and there will be an implicit conversion happening under the hood

or even

```
  implicit class DebugShowSyntax[A : Debug](self: A) { // A: Debug is a context bound
    //    def show: String = implicitly[Debug[A]].show(self)
          def show: String = Debug[A].show(self) // we have apply so we don't need implicitly
  }
```

also for `read`

```
  implicit class DebugReadSyntax(self: String) {
    def readIt[A: Debug] = Either[String, A] = Debug[A].read(self)
  }
```


** Patterns **

`Option` is broken!!! `None.get` throws an exception..

```
sealed trait Maybe[A]
final case object Empty[A] extends Maybe[A]
final case class Just[A](value: A) extends Maybe[A]
```

The `\/` operator is better than `Either` - it's in `scalaz`. Right-biased.

```
def portNumber: Error \/ Int

portNumber match {
    case -\/ (error)    => ...
    case \/- (portNo)   => ...
}
```

Validation type: `Validation[A, B]`. It can be either a `Failure(error: A)` or a `Success(value: B)`

example:

```
for {
    a <- List(1, 2, 3)
    b <- List("a", "b", "c")
} yield a.toString + 
res5: List[String] = List(1a, 1b, 1c, 2a, 2b, 2c, 3a, 3b, 3c)
```

this expands to 

```
List(1,2,3).flatMap { a =>
    List("a", "b", "c").map { b =>
        a.toString + b
    }
}
```

**monads** have `map`, `flatMap`, and `unit` (a way to lift something into a monad)


Ohter useful abstractions / typeclasses are `Semigroup` and `Monoid`.

```
trait Semigroup[A] {
    def append(a1: A, a2: A): A
}

object SemigroupLaws {
    (a append b) append c == a append (b append c) // associativity
}
```

a `Semigroup` has associative property on its operator.
e.g. String concatenation defines a Semigroup for Strings, or List.

Also `+` and `*` for integers. But **not** subtractions or divisions.

Semigroups are everywhere. Look for the associativity!!! (IO, databases...) - it's a very useful typeclass.

```
trait Monoid[A] extends Semigroup[A] { // it has to obey the associative law
    def zero: A
}

object MonoidLaws {
    a1 append zero == a1
    zero append a1 == a1  // commutative identity law
}
```

day 2

**FUNCTORS**

The kind of a `Functor` is `* => *`. `F[_]` (a bit like `List[_]`)

Typeclass:

```
trait Functor[F[_]] {
    def map[A, B](fa: F[A])(ab: A => B): F[B]
}
```

`List` is a *functor*. `List` has kind `* => *` so it works !

`Map` has kind `[*, *] => *` - so it's not compatible with `Functor` kind. But if we fix the keys e.g. `Map[String, ?]` the kind becomes `* => *`, hence you have a `Functor`.

Other examples of *functors* are `Future[_]`, `Option[_]`, `Try[_]`.

`F[A]` (or a functor) is a definition of a computation that may:

 - run forever
 - halt 
 - produce one or more `As`
 
e.g. `def some[A]: Some[A]` halt (what we can return???) but the one that runs forever is `def some[A]: Some[A] = some[A]` (infinite recursion)

Functor laws:

`map(fa)(identity) == fa` - *identity*

One way of breaking this law when mapping over a `List`? E.g. returning `null`, reordering the list, dropping elements... we say that if they obey this law it preservers the structure.

`map(map(fa)(ab))(bc) == map(fa)(ab andThen bc)` - *composition*

Nested functors are themselves functors.

`F[G[A]]` ~> `'F o G'[A]`

`(F[A], G[A])` is a functor as well. `map` is defined by the tuple of each functor's `map` function.

`Either[F[A], G[A]]` is also a functor (**coproduct** or **sum** of the functors).

Sum and nesting are very commonly used.

**LIFTING**

Going to `A` to `F[A]`. e.g. from `3` to `Some(3)`

```
trait Applicative[F[_]] extends Functor[F] {
    def point[A](a: A): F[A] // this is called return in Haskell - returns a simple value in a program!
}
```

(actually `Applicative` should extend `Apply` from the notes)

e.g. a database effect `point` will be just a fixed value.

```
trait Apply[F[_]] extends Functor[F] {
    def ap[A, B](fab: F[A => B], fa: F[A]): F[B]
}
```

for `List` we apply every `fab` to every `fa`


`Functor -> Apply -> Applicative`


Applicative functors model parallel computations, while monads represent sequential computations (because of the `bind` / `flatMap` method)

When writing instances for your own datatypes - you can choice which typeclass implement (aka how much power you're giving to the users). The more power you give to the users, the less power you have behind the scenes.


```
trait Bind[F[_]] extends Apply[F] {
    def bind[A, B](fa: F[A])(afb: A => F[B]): F[B]
}
```

(note back - a `Functor` is a program.)

`trait Monad[F[_]] extends Bind[F] with Applicative[F]`

a `Monad` squash two computations `F[A]`, `F[B]` into one. if a `Monad` represents a program, in order to produce `B` I actually have to produce `A` first (because `afb: A => F[B]`). Because of `afb` I know that the `B` I produce can be TOTALLY DIFFERENT depending on `A`!!!!

`Monad` have some laws to respect.

What is a **natural transformation**?

```
trait NaturalTransformation[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
}
```

in scalaz, cats: `F ~> G` (this is a type alias for `NaturalTransformation`)

e.g. `None` is an empty list, `Some(x)` is `List(x)`

it's not a typeclass! doesn't have any laws!!!

(see `patterns.scala`)

**Collections**

`Foldable` and `Traversable`.

```
trait Foldable[F[_]] {
    def foldMap[A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B
    def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B): B): B
}
```

`foldMap` is more simple and powerful than `foldRight` or `foldLeft`

`Future` is not `Traversable` in pure FP - you'd need to run the computation in order to perform a traversal. 

a `List` is a `Free Monoid`. (you get a monoid for free!) `coyoneda -> free functor`


**DATA**

*Ordinary data* and *codata*. 

*Data* is a finite store of information - a possibly recursive data structure (e.g. `List`). 

*Codata* is the description of a process for producing information. E.g. `Stream`. *Fibonacci* numbers for example - we're dealing with a generator of numbers, not with the finite number of information!

Processing data and codata is different.

Data is usually processed in FP using recursion. Recursion is also called **induction** in this case. As we're dealing with data and not codata, we will reduce to the basic building blocks.

**fold** is the most basic and powerful function to process data. It always terminates - because pure data is finite.

The dual of fold is **cofold** - you use it in codata to generate elements. (e.g. you have a dough ball and you extract piece by piece - there's actually infinite pieces...)

String is a **corecursive** data structure.

Is data fundamental or necessary! NO!!! Not in FP. The core is lambda calculus. 

Any data structure can be defined by a lambda - via **Church encoding**

(see `data.scala`)


the `Identity` monad is also useful!

```
val IdentityMonad = new Monad[Identity] {
    override def bind[A, B](fa: Identity[A])(f: (A) => Identity[B]): Identity[B] = f(fa.value)
    override def point[A](a: => A): Identity[A] = Identity(a)
}
```

Lots of functions can do a lot of things for `Monad`s. By using `IdentityMonad` you can give those functions everything...

`Option ~> Identity` is not possible - we'd need a value that we don't have right now. But `Identity ~> Option` is always possible!!!

If `F ~> Identity` is defined it means that `F` has at least a value (some kind of traversable....)

**Optics**

`copy` it's a nice method for case classes. but what about nested classes? nested `copy` hell!

you can use optics to explore sum and products. *monocle* is a useful library for this.

**Lens** - is used to focus on a specific element of a product (e.g. case class)

`case class Person(name: String)`

`_Name: Lens[Person, String]`

at the end of the day, a *lens* is a getter/setter really. it will return a copy.

Let's say we have nested case classes.

`case class Person(name: String, address: Adress)` 

You can use nested lenses for `Address` -> and then combine lenses! `^|->`

*Lenses* are useful for products... but what about sums? You have to use `Prism`!

`Prism[S,A]`

--- understand better

**Recursive data types**

```
sealed trait List[A]

case object Nil[A] extends List[A]
case class Cons[A](head: A, tail: List[A]) extends List[A]
```

Different ways to implement fold! (in `List` - left or right? in `Tree` - breadth or depth?)

*Recursion Schemes*

*Catamorphisms* (tearing down a structure - fold!) and *Anamorphisms* (building up a structure)

look in `data.scala` for examples

**state monad!** (see `effects.scala`)


**Reader**

`case class Reader[R, A](run: R => A)`

like a DI for functional programming!! we can use lenses to combine the effects.

Have a look also at `*T` classes in Scalaz.

**Monad transformers**

Combine monadic effects. 

`T[_[_], _]` is the generic structure of a Monad transformer. Its kind is `[* => *, *] => *` - where the first `* => *` is a Monad. This generates a monad when substituting the first Monad. `MT[_] == T[M[_], _]`

Monad transformer lay some effects uponother.

**Functional Architecture**

*free monads* are an alternative to monad transformers.

How could we run programs without thunk? In a pure FP world?

We can *describe the interpretation*, and defer the execution to something else.

`def println(s: String): Unit` it's a computation.

```
sealed trait Console
case class Println(s: String) extends Console
case class Ser(first: Console, second: Console) extends Console
```

it's a description of a computation - or a DSL to build strings?

`architecture.scala`


try to describe effects with a *minimal set* of interactions.

What can we use Free monads for?

- unit testing
- mocking
- aspects
- separation of concerns => ONION architecture.
- optimization


Good example of code to look at:

- Quasar-analytics/Quasar
- Doobie
- FreeK
- Fcats
