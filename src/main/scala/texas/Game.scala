package texas

import texas.ComboModel.{Combo, Flush, Four, FullHouse, High, Pair, Straight, StraightFlush, Three, TwoPair}
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.either._

object Game {
  def apply(raw: String): Either[String, String] = {
    val arrayRaw = raw.split(" ").filter(_.nonEmpty)

    if (arrayRaw.length < 3) Left("too less parts for a game")
    else if (arrayRaw(0) == "texas-holdem") {
      val eitherBoard = arrayRaw(1).grouped(2).toList.map(Card(_)).sequence
      val eitherHands = arrayRaw.drop(2).map {
        rawHand =>
          rawHand.grouped(2).toList
            .map(Card(_)).sequence
            .flatMap {
              cards =>
                if (cards.size != 2) Left(s"bad cards to create a hand [${cards.mkString("")}]")
                else Right(Hand(cards.head, cards(1)))
            }
      }.toList.sequence

      for {
        cardsBoard <- eitherBoard
        hands <- eitherHands
        board <- Board(cardsBoard)
        finalOrderedHands <- Game(board, hands: _*)
      } yield finalOrderedHands
    } else Left(s"${arrayRaw(0)} isn't supported")
  }

  def apply(board: Board, hands: Hand*): Either[String, String] = {
    if (hands.size < 2) Left("game must have minimum 2 players")
    else {
      val totalHands =
        board.card1 ::
        board.card2 ::
        board.card3 ::
        board.card4 ::
        board.card5 ::
        hands.flatMap(h => List(h.card1, h.card2)).toList

      if (totalHands != totalHands.distinct) Left("game must have distinct cards")
      else {
        val sortedCombos = hands.map(hand => hand -> bestCombo(board, hand)).sortBy(_._2)
        Right(printResult(sortedCombos.toList).reverse.mkString(" "))
      }
    }
  }

  private def printResult(list: List[(Hand, Combo)], result: List[String] = Nil): List[String] = {
    list match {
      case head :: rest =>
        val equalHands = head._1 :: rest.takeWhile(r => r._2.compare(head._2) == 0).map(_._1)
        printResult(list.drop(equalHands.size), equalHands.reverse.mkString("=") :: result)
      case Nil => result
    }
  }

  def bestCombo(board: Board, hand: Hand): Combo =
    StraightFlush(board, hand)
      .orElse(Four(board, hand))
      .orElse(FullHouse(board, hand))
      .orElse(Flush(board, hand))
      .orElse(Straight(board, hand))
      .orElse(Three(board, hand))
      .orElse(TwoPair(board, hand))
      .orElse(Pair(board, hand))
      .getOrElse(High(board, hand))
}
