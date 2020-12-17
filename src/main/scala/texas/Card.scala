package texas

import Card._

case class Card private[Card](suit: Suit, rank: Rank) extends Ordered[Card] {
  val rankInt = order(rank)
  override def compare(that: Card) = rankInt.compare(that.rankInt)
  override def toString = s"$rank$suit"
}

object Card {
  type Rank = String
  type Suit = String

  def apply(raw: String): Either[String, Card] = {
    if (
      raw.length == 2 &&
        possibleSuits.contains(raw(1)) &&
        order.contains(raw.take(1))
    ) Right(new Card(raw.slice(1, 2), raw.take(1)))
    else Left(s"cannot parse a given string [$raw] to Card")
  }

  private val possibleSuits = "cdhs"
  private val order = Map(
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9,
    "T" -> 10,
    "J" -> 11,
    "Q" -> 12,
    "K" -> 13,
    "A" -> 14
  )
}