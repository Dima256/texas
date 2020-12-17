package texas

case class Board(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card)

object Board {
  def apply(list: List[Card]): Either[String, Board] =
    if (list.size == 5) Right(new Board(list.head, list(1), list(2), list(3), list(4)))
    else Left(s"board must have 5 cards, not ${list.size}")
}
