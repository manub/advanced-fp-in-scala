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
  final case class IO[A](unsafePerformIO: () => A)

  def readLine2: IO[String] = IO(() => readLine()) //no matter how many times you run this, it will **always** return a description of a computation

  def printLine(line: String): IO[Unit] = IO(() => println(line))

  implicit val MonadIO: Monad[IO] = new Monad[IO] {
    override def bind[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] =
    IO(() => f(fa.unsafePerformIO()).unsafePerformIO())
//    f(fa.unsafePerformIO()) // non correct - f is called outside IO
    override def point[A](a: => A): IO[A] = IO(() => a)
  }

  val program: IO[Unit] = for {
    _ <- printLine("what is your name?")
    name <- readLine2
    _ <- printLine(s"Hello $name!")
  } yield ()

  def main(args: Array[String]) = program.unsafePerformIO()
}

object exercise4 {
  def filter[A](f: A => Boolean, l: List[A]): List[A] = {
    val foldable = Foldable[List]

    foldable.foldMap[A, List[A]](l)(a => if (f(a)) List(a) else Nil)
  }
}

object exercise5 {

  // An example of Coyoneda...

//  trait DeferMap[F[_], B] {
//    type A
//    val fusedMap: A => B
//    val fa: F[A]
//  }
//  object DeferMap {
//    def defer[F[_], A0](fa0: F[A0]): DeferMap[F, A0] = new DeferMap[F, A0] {
//      type A = A0
//      val fusedMap: A0 => A = identity
//      val fa = fa0
//    }
//
//    implicit def DeferMapFunctor[F[_]]: Functor[DeferMap[F, ?]] = new Functor[DeferMap[F, ?]] {
//      override def map[A0, B0](fa: DeferMap[F, A0])(f: A0 => B0): DeferMap[F, B0] = new DeferMap[F, B0] {
//        type A = fa.A
//        val fusedMap = fa.fusedMap.andThen(f)
//        val fa = fa.fa
//      }
//    }
//
//    val deferMap = defer[List, Int](List(1,2,3,4))
//
//   MAP FUSION!!!!
//  }


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
