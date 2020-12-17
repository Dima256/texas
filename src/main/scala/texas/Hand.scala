package texas

case class Hand(card1: Card, card2: Card) {
  override def toString: String = s"$card1$card2"
}
