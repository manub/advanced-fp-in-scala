package lambdaconf.architecture

import matryoshka._
import monocle._
import scalaz._

import Scalaz._


object exercise0 {



  sealed trait Console[A]
  case class Println(value: String) extends Console[Unit]
  case class Readln[A](f: String => A) extends Console[A]
  case class Bind[A, B](first: Console[A], second: A => Console[B]) extends Console[B] // => this is bind!!!


  // ALMOST a monad... but we don't have point...

  case class Return[A](value: A) extends Console[A] // this is point

  val program: Console[Unit] = Bind(
    Println("Hello, what's your name?"),
    _ => Bind(
      Readln(line => line),
      name => Println(s"Hello $name")
    )
  )

  implicit val MonadConsole: Monad[Console] = new Monad[Console] {
    override def bind[A, B](fa: Console[A])(f: (A) => Console[B]): Console[B] = Bind(fa, f)
    override def point[A](a: => A): Console[A] = Return(a)
  }

  // rewrite program!

  for {
    _    <- Println("Hello, what's your name?")
    name <- Readln(identity)
    _     <- Println(s"Hello $name")
  } yield ()

  // this makes me able to do functional mocking! I haven't even used a real console.


  // THE FREE MONAD!!! behold

//  sealed trait Free[F[_], A]
//  case class Return[F[_], A](value: A) extends Free[F, A]
//  case class Bind[F[_], A, B](first: Free[F, A], second: A => Free[F, B]) extends Free[F, B]  <- naive implementation of free

  def printLine(line: String): Free[Console, Unit] = Free.liftF(line)
  def readLine: Free[Console, String] = Free.liftF(Readln)

  sealed trait FileIO[A]
  case class Ls(path: String) extends FileIO[List[String]]

  sealed trait Logging[A]
  case class Log(level: String, message: String)

  val program2: Free[Console[_] :+: FileIO[_] :+: Logging[_], Unit]

  val eliminateLogging: Logging ~> FileIO

  val program3: Free[Console :+: FileIO, Unit]

}
object exercise1 {
  final case class CashAmount()
  final case class AtmError(message: String)
  final case class TransactionID(identifier: String)
  final case class AccountID(identifier: String)

  trait Atm[F[_]] {
    def withdraw(account: AccountID, amount: CashAmount): F[Either[AtmError, TransactionID]]

    def deposit(account: AccountID, amount: CashAmount): F[TransactionID]

    def accounts: F[NonEmptyList[AccountID]]

    def transfer(from: AccountID, to: AccountID, amount: CashAmount): F[Either[AtmError, TransactionID]]

    def balance(account: AccountID): F[CashAmount]
  }
  object Atm {
    def apply[F[_]](implicit atm: Atm[F]): Atm[F] = atm

    implicit def AtmFree[F[_]: Atm]: Atm[Free[F, ?]] = new Atm[Free[F, ?]] {
      type G[A] = Free[F, A]

      def withdraw(account: AccountID, amount: CashAmount): G[Either[AtmError, TransactionID]]
        = Free.liftF(Atm[F].withdraw(account, amount))

      def deposit(account: AccountID, amount: CashAmount): G[TransactionID]
        = Free.liftF(Atm[F].deposit(account, amount))

      def accounts: G[NonEmptyList[AccountID]]
        = Free.liftF(Atm[F].accounts)

      def transfer(from: AccountID, to: AccountID, amount: CashAmount): G[Either[AtmError, TransactionID]]
        = Free.liftF(Atm[F].transfer(from, to, amount))

      def balance(account: AccountID): G[CashAmount]
        = Free.liftF(Atm[F].balance(account))
    }
  }
  sealed trait AtmF[A]
  object AtmF {
    final case class Withdraw(account: AccountID, amount: CashAmount) extends AtmF[Unit]
    // ???
  }
  implicit val AtmAtmF: Atm[AtmF] = ???
}

object exercise2 {
  sealed trait LogLevel
  object LogLevel {
    final case object Info extends LogLevel
    final case object Debug extends LogLevel
  }

  trait Logging[F[_]] {
    def log(level: LogLevel, message: String): Free[F, Unit]
  }
  sealed trait LoggingF[A]
  object LoggingF {
    final case class Log(level: LogLevel, message: String) extends LoggingF[Unit]
    // ???
  }
  implicit val LoggingLoggingF: Logging[LoggingF] = ???
}

object exercise3 {
  import exercise1._
  import exercise2._

  def logAtm: AtmF ~> LoggingF = ???
}

object exercise4 {
  import exercise1._
  import exercise2._
  import exercise3._

  final case class Bank(/* ??? */)
  final case class Log(/* ??? */)

  def interpretAtm: AtmF ~> State[Bank, ?] = ???

  def interpretLog: LoggingF ~> State[Log, ?] = ???

  type Final[A] = State[(Bank, Log), A]

  def interpretProgram: AtmF ~> Final = ???

  def exampleProgram[F[_]: Monad](implicit atm: Atm[F]): F[CashAmount] = for {
    accounts <- atm.accounts
    _        <- atm.deposit(accounts.head, CashAmount())
    balance  <- atm.balance(accounts.head)
  } yield balance

  val exampleBalance: CashAmount =
    exampleProgram[Free[AtmF, ?]].foldMap[Final](interpretProgram).eval((Bank(), Log()))
}

object exercise5 {
  import exercise1._
  import exercise2._
  import exercise3._
  import exercise4._

  trait Console[F[_]] {
    def readLine: F[String]

    def println(line: String): F[Unit]
  }
  object Console {
    def apply[F[_]](implicit console: Console[F]): Console[F] = console

    implicit def ConsoleFree[F[_]: Console]: Console[Free[F, ?]] = new Console[Free[F, ?]] {
      type G[A] = Free[F, A]

      def readLine: G[String] = Free.liftF(Console[F].readLine)

      def println(line: String): G[Unit] = Free.liftF(Console[F].println(line))
    }
  }
  sealed trait ConsoleF[A]
  object ConsoleF {
    final case object ReadLine extends ConsoleF[String]
    // ???
  }
  implicit val ConsoleConsoleF: Console[ConsoleF] = ???

  sealed trait ConsoleLogging[F[_]] extends Console[F] with Logging[F]

  sealed trait ConsoleLoggingF[A]
  object ConsoleLoggingF {
    final case class ConsoleTerm[A](op: ConsoleF[A]) extends ConsoleLoggingF[A]
    final case class LoggingTerm[A](op: LoggingF[A]) extends ConsoleLoggingF[A]
  }

  implicit val ConsoleLoggingConsoleLoggingF: ConsoleLogging[ConsoleLoggingF] = ???

  def program: Free[ConsoleLoggingF, Unit] = ???
}
