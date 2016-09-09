package lambdaconf.functions

object exercise1 {
  // Domain: {Vegetables, Fruits, Meat, Dairy, Eggs}
  // Codomain: {Love, Like, Neutral, Dislike, Hate}
}

object exercise2 {
  val compareStrings: (Char => Char) => (String, String) => Boolean = {
    normalisation => (a, b) => a.map(normalisation) == b.map(normalisation)
  }
}

object exercise3 {
  type Parser[A] = String => Either[String, (String, A)]

  def or[A](left: Parser[A], right: Parser[A]): Parser[A] =
    s => left(s) match {
      case Left(error) => right(s)
      case x => x
    } // you can write flatmap or left projection as well

  def sequence[A, B](first: Parser[A], second: Parser[B]): Parser[(A, B)] = ???
}

object exercise4 {
  object snd {
    def apply[A, B](v: (A,B)): B = v._2
  }
//  def snd[A, B](v: (A, B)): B = v._2


  // my own implementation
  trait Boolean { // doesn't need to be sealed!!! there's only two possible ways of building Boolean with this signature...
    def apply[A](v: (A, A)) : A
  }
  object True extends Boolean {
    def apply[A](v: (A, A)) : A = v._1
  }
  object False extends Boolean {
    def apply[A](v: (A, A)) : A = v._2
  }
}
