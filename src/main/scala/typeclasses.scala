package lambdaconf.typeclasses

import scala.util.Try

object exercise1 {

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

//  implicit class DebugShowSyntax[A](self: A)(implicit ev: Debug[A]) {
//    def show: String = ev.show(self)
//  }

  implicit class DebugShowSyntax[A : Debug](self: A) { // A: Debug is a context bound
//    def show: String = implicitly[Debug[A]].show(self)
    def show: String = Debug[A].show(self)
  }
  implicit class DebugReadSyntax(self: String) {
    def readIt[A: Debug]: Either[String, A] = Debug[A].read(self)
  }

  def main(args: Array[String]) = {
    println(1.show)  // this works for the implicit conversion
    println("1".readIt)
  }

  sealed trait PathLike[A] {
    def concat(first: A, second: A): A
    def root: A

  }

  object PathLike {
    def apply[A: PathLike]: PathLike[A] = implicitly[PathLike[A]]
  }
}

object exercise2 {
  import exercise2._
  // LAWS
  // concat(concat(a, b), c) == concat(a, concat(b, c))
  // concat(root, a) == a
  // concat(a, root) == a
}

object exercise3 {
  import exercise1._
  import exercise2._

  sealed trait Node
  final case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node

  object Node {
    // example implementation
    implicit val NodePathLike: PathLike[Node] = new PathLike[Node] {
      override def concat(first: Node, second: Node): Node = (first, second) match {
        case (Root, y) => y
        case (x, Root) => x
        case (x, Child(p2, n2)) => Child(concat(x, p2), n2)
      }
      override def root: Node = Root
    }
  }
}

object exercise4 {
  import exercise1._
  import exercise2._
  import exercise3._

  implicit class PathLikeSyntax[A: PathLike](self: A) {
    def /(next: A): A = PathLike[A].concat(self, next)
  }

  def root[A: PathLike]: A = implicitly[PathLike[A]].root // or PathLike[A].root
}
