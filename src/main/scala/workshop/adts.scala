package workshop

object adts {

  // Design a data type for coffee sizes, should have small medium and large
  sealed trait Size
  case object Small extends Size
  case object Medium extends Size
  case object Large extends Size

  // Model a data type for a contact that can either be an email or a phone number
//  type Contact = Unit

  sealed trait Contact
  final case class Email(emailAddress: String) extends Contact
  final case class PhoneNumber(number: String) extends Contact


  // Design a data type for a chess piece and its position on the chess board
//  type ChessPiece = Unit
  case class BoardPosition(vertical: Int, horizontal: Int)

  sealed trait ChessPiece {
    def position: BoardPosition
  }
  case class Queen(position: BoardPosition) extends ChessPiece
  case class Pawn(position: BoardPosition) extends ChessPiece
  case class Knight(position: BoardPosition) extends ChessPiece
  case class Bishop(position: BoardPosition) extends ChessPiece
  case class King(position: BoardPosition) extends ChessPiece
  case class Rook(position: BoardPosition) extends ChessPiece

  // Write a function using pattern mathcing that takes a square and returns whether it can move there or not
  def canMove(piece: ChessPiece, targetPosition: BoardPosition): Boolean = {
    val horizontalDiff  = Math.abs(piece.position.horizontal - targetPosition.horizontal)
    val verticalDiff    = Math.abs(piece.position.vertical   - targetPosition.vertical)

    piece match {
      case King(_) => {
        (horizontalDiff == 1 && verticalDiff == 0) ||
        (verticalDiff == 1 && horizontalDiff == 0) ||
        (horizontalDiff == 1 && verticalDiff == 1)
      }
      case Bishop(_)  => horizontalDiff - verticalDiff == 0
      case Rook(_)    => horizontalDiff == 0 || verticalDiff == 0
      case Queen(_)   => {
        (horizontalDiff - verticalDiff == 0) ||
        (horizontalDiff == 0 || verticalDiff == 0)
      }
      case Knight(_)  => {
        (horizontalDiff == 2 && verticalDiff == 1) ||
        (horizontalDiff == 1 && verticalDiff == 2)
      }
      case Pawn(_)  => {
        (horizontalDiff == 1 && verticalDiff == 0) ||
        (verticalDiff == 1 && horizontalDiff == 0)
      }
    }
  }

  // Model a data type that stores time spans
//  type TimeSpan = Unit
  sealed trait TimeUnit
  case object Seconds extends TimeUnit
  case object Minutes extends TimeUnit
  case object Hours extends TimeUnit

  case class TimeSpan(duration: Int, unit: TimeUnit) {
    def durationInSeconds: Int = unit match {
      case Hours => duration * 3600
      case Minutes => duration * 60
      case Seconds => duration
    }
  }

  def add(first: TimeSpan, second: TimeSpan): TimeSpan = {
    if (first.unit == Hours && second.unit == Hours)
      TimeSpan(first.duration + second.duration, Hours)
    else if (first.unit == Minutes && second.unit == Minutes)
      TimeSpan(first.duration + second.duration, Minutes)
    else
      TimeSpan(first.durationInSeconds + second.durationInSeconds, Seconds)
  }

  def smallestUnit(first: TimeUnit, second: TimeUnit): TimeUnit =
    (first, second) match {
      case (Seconds, _) | (_, Seconds) => Seconds
      case (Minutes, _) | (_, Minutes) => Minutes
      case (Hours, _) | (_, Hours) => Minutes
    }

  // Write a function that adds two TimeSpan values together

  // List all values of the type `Unit`
  def allValuesUnit: Set[Unit] = Set(())

  // List all values of the type `Nothing`
  def allValuesNothing: Set[Nothing] = Set.empty

  // List all values of the type `Boolean`
  def allValuesBoolean: Set[Boolean] = Set(false, true)

  // List all values of the type `Size`
  def allValuesSize: Set[Size] = Set(Small, Medium, Large)

  // List all values of the type `(Size, Boolean)`
  def allValuesTuple: Set[(Size, Boolean)] =
    Set(
      (Small, false),
      (Small, true),
      (Medium, false),
      (Medium, true),
      (Large, false),
      (Large, true),
    )

  // List all values of the type `Either[Size, Boolean]`
  def allValuesEither: Set[Either[Size, Boolean]] =
    Set(
      Left(Small),
      Left(Medium),
      Left(Large),
      Right(false),
      Right(true),
    )

  // List all values of the type `(Size, Unit)`
  def allValuesTupleUnit: Set[(Size, Unit)] =
    Set(
      (Small, ()),
      (Medium, ()),
      (Large, ()),
    )

  // List all values of the type `Either[Boolean, Nothing]`
  def allValuesEitherNothing: Set[Either[Boolean, Nothing]] =
    Set(
      Left(false),
      Left(true),
    )

}
