package lambdaconf.types

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
//  final case class CheckersBoard(board:)

  sealed trait SquareState
  case object White extends SquareState
  case object Black extends SquareState
 // model it as product and sum types
  // ???
}

object exercise2 {
  final case class Box[A](a: A)
}

object exercise3 {
  // 1. scala.collection.List * => *
  // 2. F[_, _] [*, *] => *
  // 3. Option * => *
  // 4. Int *
  // 5. T[_[_], _] // [(* => *), *] => *
}

object exercise4 {
  trait FileSystem {
    type File

    def ls: List[File]
  }

  val fs: FileSystem = ???
  val myFiles: List[FileSystem#File] = fs.ls
}

object exercise5 {
  sealed trait Example[F[_]] {
    def value: F[String]
  }

  type L[A] = Either[A, Int]

  new Example[L] { // <-- ???
    def value: Either[String, Int] = Right(2)
  }
}
