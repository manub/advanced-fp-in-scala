package lambdaconf.patterns

import matryoshka._
import monocle._

import scalaz._
import Scalaz._
import scala.util.Try

object lesson {
  sealed trait Error
  def readPortNumber: Error \/ Int = ???
  def readHostname  : Error \/ String = ???

  for {
    portNumber <- readPortNumber
    hostname <- readHostname
//  } yield (portNumber, hostname): Error \/ (Int, String) // ??
  } yield (portNumber, hostname): (Int, String) // ??
}

object exercise1 {
  sealed trait Error
  final case class NotANumber(value: String) extends Error

  def readRowCol(): Error \/ (Int, Int) = {

    def toInt(s: String): Error \/ Int = \/.fromTryCatchNonFatal(s.toInt).leftMap(_ => NotANumber(s))

    println("Please enter a row:")
    val row = readLine()
    println("Please enter a column:")
    val col = readLine()

    for {
      row <- toInt(row)
      col <- toInt(col)
    } yield (row, col)
  }

  def readRowCol2(): ValidationNel[Error, (Int, Int)] = { // validation non empty list

    def toInt(s: String): ValidationNel[Error, Int] =
      Validation.fromTryCatchNonFatal(s.toInt).leftMap(_ => NonEmptyList(NotANumber(s)))

    println("Please enter a row:")
    val row = readLine()
    println("Please enter a column:")
    val col = readLine()

    (toInt(row) |@| toInt(col))((_, _)) // applicative builder
  }
}

object exercise2 {
  sealed trait Node
  final case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node

  implicit val NodeMonoid: Monoid[Node] = new Monoid[Node] {
    override def zero: Node = Root
    override def append(f1: Node, f2: => Node): Node = (f1, f2) match {
      case (Root, y) => y
      case (x, Root) => x
      case (x, Child(p2, n2)) => Child(append(x, p2), n2)
    }
  }
}

object exercise3 {
  final case class Thunk[A](run: () => A)

  implicit val MonadThunk: Monad[Thunk] = ???
}

object exercise4 {
  def filter[A](f: A => Boolean, l: List[A]): List[A] = {
    val foldable = Foldable[List]

    ???
  }
}

object exercise5 {
  trait List[A] { self =>
    def fold[Z](nil: => Z, cons: (Z, A) => Z): Z

    final def :: (next: A): List[A] = new List[A] {
      def fold[Z](nil: => Z, cons: (Z, A) => Z): Z = {
        cons(self.fold(nil, cons), next)
      }
    }
  }
  object List {
    def empty[A]: List[A] = new List[A] {
      def fold[Z](nil: => Z, cons: (Z, A) => Z): Z = nil
    }
  }

  implicit val ListTraverse: Traverse[List] = ???
}
