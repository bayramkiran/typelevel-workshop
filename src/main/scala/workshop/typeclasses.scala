package workshop

import workshop.adts._
import workshop.model.rawText
import simulacrum.typeclass

import scala.concurrent.Future
import workshop.typeclasses.Show.ops._
import workshop.typeclasses.Eq.ops._
import workshop.typeclasses.Monoid.ops._
import workshop.typeclasses.Functor.ops._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

object typeclasses {

  //Show

  @typeclass trait Show[A] {
    def show(a: A): String
  }

  implicit def showInt: Show[Int] = new Show[Int] {
    def show(a: Int): String = a.toString
  }

  implicit def showString: Show[String] = new Show[String] {
    def show(a: String): String = a
  }

  implicit def showChessPiece: Show[ChessPiece] = piece => {
    s"Position -> x: ${piece.position.horizontal} y: ${piece.position.vertical}"
  }

  implicit def showOption[A: Show]: Show[Option[A]] = new Show[Option[A]] {
    def show(a: Option[A]): String = a.map(_.show).getOrElse("")
  }




  //Eq

  @typeclass trait Eq[A] {
    def eqv(x: A, y: A): Boolean
    def ===(x: A)(y: A): Boolean = eqv(x, y)
  }

  implicit def eqInt: Eq[Int] = new Eq[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y
  }

  implicit def eqString: Eq[String] = new Eq[String] {
    def eqv(x: String, y: String): Boolean = x.equals(y)
  }

  implicit def eqOption[A: Eq]: Eq[Option[A]] = ???

  implicit def eqEither[A: Eq, B: Eq]: Eq[Either[A, B]] = new Eq[Either[A, B]] {
    def eqv(x: Either[A, B], y: Either[A, B]): Boolean =
      (x, y) match {
        case (Left(thisA),  Left(thatA))  => thisA === thatA
        case (Right(thisB), Right(thatB)) => thisB === thatB
        case _                            => false
      }
  }





  //Monoid

  @typeclass trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A

    def |+|(x: A)(y: A): A = combine(x, y)
  }

  implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  implicit def stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }


  implicit def timespanMonoid: Monoid[TimeSpan] = new Monoid[TimeSpan] {
    override def empty: TimeSpan = TimeSpan(duration = 0, unit = Seconds)

    override def combine(x: TimeSpan, y: TimeSpan): TimeSpan = add(x, y)
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def empty: List[A] = List.empty

    override def combine(x: List[A], y: List[A]): List[A] = x ::: y
  }

  // The intMonoid further up uses `addition` as its method of combination, but that is not the only monoid for `Int`!
  // We can also define one for multiplication, though if we just define it for `Int` again the compiler won't know which to pick
  // What we can do instead is define a small wrapper that allows us to multiply
  case class Mult(value: Int)

  implicit def multMonoid: Monoid[Mult] = new Monoid[Mult] {
    override def empty: Mult = Mult(1)

    override def combine(x: Mult, y: Mult): Mult = Mult(x.value * y.value)
  }

  def combineAll[A: Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)(_ combine _)

  def foldMap[A, B: Monoid](list: List[A])(f: A => B): B =
    list.foldLeft(Monoid[B].empty)((acc, cur) => acc combine f(cur))

  implicit def tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] = new Monoid[(A, B)] {
    override def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)

    override def combine(x: (A, B), y: (A, B)): (A, B) = (x._1 combine y._1, x._2 combine y._2)
  }

  implicit def tuple3Monoid[A: Monoid, B: Monoid, C: Monoid]: Monoid[(A, B, C)] = new Monoid[(A, B, C)] {
    override def empty: (A, B, C) = (Monoid[A].empty, Monoid[B].empty, Monoid[C].empty)

    override def combine(x: (A, B, C), y: (A, B, C)): (A, B, C) =
      (x._1 combine y._1, x._2 combine y._2, x._3 combine y._3)
  }

  implicit def mapMonoid[A, B: Monoid]: Monoid[Map[A, B]] = new Monoid[Map[A, B]] {
    override def empty: Map[A, B] = Map.empty

    override def combine(x: Map[A, B], y: Map[A, B]): Map[A, B] =
      x.foldLeft(y) { (acc: Map[A, B], cur: (A, B)) =>
        val (curKey, curVal) = cur
        val updated = acc.get(curKey).map(curVal combine _).getOrElse(curVal)

        acc.updated(curKey, updated)
      }
  }

  implicit def futureMonoid[A: Monoid]: Monoid[Future[A]] = new Monoid[Future[A]] {
    override def empty: Future[A] = Future.successful(Monoid[A].empty)

    override def combine(x: Future[A], y: Future[A]): Future[A] =
      for {
        a1 <- x
        a2 <- y
      } yield a1 combine a2
  }


  //Monoid word count
  //Use foldMap with a Monoid to count the number of words, the number of characters and the number of occurences of each word
  //Tip: the Map and Tuple3 Monoid can help

  val words: List[String] = rawText.split(" ").toList

  val wordCounts: (Int, Int, Map[String, Int]) = foldMap(words)(word => (1, word.length, Map(word -> 1)))


  //Now that you have the word count let's extend it with the ability to get the longest word of the text.
  //Tip: Define a Maximum Monoid to do so
  case class Max(word: String)

  implicit def maxMonoid: Monoid[Max] = new Monoid[Max]{
    override def empty: Max = Max("")

    override def combine(x: Max, y: Max): Max =
     if(x.word.length >= y.word.length) x
     else y
  }

  foldMap(words)(word => Max(word))

  //Functor

  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit def optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
  }

  implicit def listFunctor: Functor[List] = new Functor[List] {

    override def map[A, B](fa: List[A])(f: A => B): List[B] = {
      @tailrec def mapNil(as: List[A], baseValue: List[B] = Nil): List[B] = as match {
        case Nil          => baseValue
        case head :: tail => mapNil(tail, List(f(head)))
      }

      mapNil(fa)
    }
  }



  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(a) => Leaf(f(a))
        case Branch(t1, t2) => Branch(map(t1)(f), map(t2)(f))
      }
  }






  //Cardinality

  @typeclass trait Cardinality[A] {
    def cardinality: BigInt
  }

  implicit def cardinalityUnit: Cardinality[Unit] = new Cardinality[Unit] {
    override def cardinality: BigInt = BigInt(1)
  }

  implicit def cardinalityBoolean: Cardinality[Boolean] = new Cardinality[Boolean] {
    override def cardinality: BigInt = BigInt(2)
  }

  implicit def cardinalityByte: Cardinality[Byte] = new Cardinality[Byte] {
    override def cardinality: BigInt = BigInt(256)
  }

  implicit def cardinalityShort: Cardinality[Short] = new Cardinality[Short] {
    override def cardinality: BigInt = BigInt(2).pow(16)
  }

  implicit def cardinalityInt: Cardinality[Int] = new Cardinality[Int] {
    override def cardinality: BigInt = BigInt(2).pow(32)
  }

  implicit def cardinalityTuple[A: Cardinality, B: Cardinality]: Cardinality[(A, B)] = new Cardinality[(A, B)] {
    override def cardinality: BigInt = Cardinality[A].cardinality * Cardinality[B].cardinality
  }

  implicit def cardinalityEither[A: Cardinality, B: Cardinality]: Cardinality[Either[A, B]] = new Cardinality[Either[A, B]] {
    override def cardinality: BigInt = Cardinality[A].cardinality + Cardinality[B].cardinality
  }

  implicit def cardinalitySize: Cardinality[Size] = new Cardinality[Size] {
    override def cardinality: BigInt = BigInt(3)
  }

  implicit def cardinalityNothing: Cardinality[Nothing]= new Cardinality[Nothing] {
    override def cardinality: BigInt = BigInt(0)
  }

//  implicit def cardinalityFunction[A: Cardinality, B: Cardinality]: Cardinality[A => B] = ???



}
