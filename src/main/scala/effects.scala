package lambdaconf.effects

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
  final case class IO[A](/* ??? */)

  implicit val IOMonad: Monad[IO] = ???
}

object exercise2 {
  import exercise1._

  sealed trait ConsoleF[A]

  val program: Free[ConsoleF, Unit] = ???
}

object exercise3 {
  final case class State[S, A](run: S => (S, A)) {
    def evalState(s: S): A = run(s)._2
  }

  implicit def StateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
    override def bind[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] =
      State[S, B] { s0 =>
        val (s1, a) = fa.run(s0) // run the first step
        f(a).run(s1) // run the second step, passing the result of the first step
      }

    override def point[A](a: => A): State[S, A] = State[S, A](s => (s, a))
  }

  object State {
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
    def modify[S](f: S => S): State[S, S] = State(s => (f(s), s))
  }

  case class Person(name: String, age: Int)

  type StateP[A] = State[Person, A]

  val incrementAge: StateP[Unit] =
    for {
      p <- State.get[Person]
      _ <- State.set[Person](p.copy(age = p.age + 1))
    } yield ()

  incrementAge.run(Person("John", 30))

  case class StateT[S, F[_], A](run: S => F[(S, A)]) // combine state with F monadic effect

  implicit def MonadStateT[F[_]: Monad] = ???

  case class Config()
  type Program[A] = StateT[Person, Reader[Config, ?], A]

}

object exercise4 {
  import exercise3._

  def sort[A: Order](list: List[A]): List[A] =
    (??? : State[List[A], List[A]]).evalState(list)
}
