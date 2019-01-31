package workshop

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.forAll
import workshop.typeclasses.Monoid.ops._
import workshop.typeclasses.Eq.ops._
import workshop.adts._
import workshop.typeclasses.{Eq, Monoid}

object properties extends Properties("Workshop") {

  def boardPositionGen: Gen[BoardPosition] =
    for {
      x <- Gen.choose(1, 8)
      y <- Gen.choose(1, 8)
    } yield BoardPosition(x, y)



  // Implement a Generator for generating only pawns
  def pawnGen: Gen[ChessPiece] =
    boardPositionGen.flatMap(Pawn.apply)

  // Write a Generator that generates kings and queens
  def kingQueenGen: Gen[ChessPiece] = Gen.oneOf(
    boardPositionGen.flatMap(King.apply),
    boardPositionGen.flatMap(Queen.apply),
  )

  // Implement a generator rooks, bishops and knights
  def rookBishopKnightGen: Gen[ChessPiece] = Gen.oneOf(
    boardPositionGen.map(pos => Rook(pos)),
    boardPositionGen.map(pos => Bishop(pos)),
    boardPositionGen.map(pos => Knight(pos)),
  )


  // Implement an Arbitrary instance for ChessPiece using the generators earlier
  implicit def chessPieceArbitrary: Arbitrary[ChessPiece] = Arbitrary[ChessPiece] {
    Gen.oneOf(
      pawnGen,
      kingQueenGen,
      rookBishopKnightGen
    )
  }




  // write a property that checks that Pawns can only move forward
  // to do so we'll use the generator that generates pawns only
  property("Pawns move forward") = Prop.forAll(pawnGen) { (pawn: ChessPiece) =>
    canMove(pawn, pawn.position.copy(horizontal = pawn.position.horizontal + 1))
  }

  property("Pawns move sideways") = Prop.forAll(pawnGen) { (pawn: ChessPiece) =>
    canMove(pawn, pawn.position.copy(vertical = pawn.position.vertical + 1))
  }


  // Next let's think about the various type class laws we learned about, can you implement properties for them?
  def monoidAssociativity[M: Monoid: Arbitrary: Eq] =
    property("Associativity") = Prop.forAll { (m1: M, m2: M, m3: M) =>
      ((m1 |+| m2) |+| m3) === (m1 |+| (m2 |+| m3))
    }

  def monoidIdentity[M: Monoid: Arbitrary: Eq] =
    property("Identity") = Prop.forAll { (m: M) =>
      (m |+| Monoid[M].empty) === m
    }

  monoidAssociativity[String]
  monoidIdentity[String]
}
