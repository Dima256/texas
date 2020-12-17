package texas

object ComboModel {

  sealed trait Combo extends Ordered[Combo] {
    val rank: Int
  }

  case class StraightFlush(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combo {
    val isA2345 = isLowStraight(card1, card2, card3, card4, card4)

    override val rank = 9

    override def compare(that: Combo): Int =
      that match {
        case sf: StraightFlush =>
          if (isA2345 && sf.isA2345) 0
          else if (isA2345 && !sf.isA2345) -1
          else if (!isA2345 && sf.isA2345) 1
          else
            compareListCards(
              List(card1, card2, card3, card4, card5),
              List(sf.card1, sf.card2, sf.card3, sf.card4, sf.card5)
            )
        case _ => 1
      }
  }

  object StraightFlush {
    def apply(board: Board, hand: Hand): Option[StraightFlush] = {
      bunch(board, hand).groupBy(_.suit).collectFirst {
        case (_, list) if list.size >= 5 => list
      }.flatMap {
        list =>
          findBest5StraightRow(list)
            .collect {
              case List(card1, card2, card3, card4, card5) =>
                new StraightFlush(card1, card2, card3, card4, card5)
            }
      }
    }
  }

  case class Four(card1: Card, card2: Card, card3: Card, card4: Card, kicker1: Card) extends Combo {
    override val rank = 8

    override def compare(that: Combo): Int =
      that match {
        case f: Four => kicker1.compare(f.kicker1)
        case _ => 1
      }
  }

  object Four {
    def apply(board: Board, hand: Hand): Option[Four] = {
      groupByBestRank(bunch(board, hand), 4)
        .collectFirst {
          case (_, List(card1, card2, card3, card4)) =>
            val List(kicker) = findKickers(List(card1, card2, card3, card4), board, hand, 1)
            new Four(card1, card2, card3, card4, kicker)
        }
    }
  }

  case class FullHouse(card1Three: Card, card2Three: Card, card3Three: Card, card4Pair: Card, card5Pair: Card) extends Combo {
    override val rank = 7

    override def compare(that: Combo): Int =
      that match {
        case fh: FullHouse =>
          val result = card1Three.compare(fh.card1Three)
          if (result == 0) card4Pair.compare(fh.card4Pair) else result
        case _ => rank.compare(that.rank)
      }
  }

  object FullHouse {
    def apply(board: Board, hand: Hand): Option[FullHouse] =
      groupByBestRank(bunch(board, hand), 3)
        .collectFirst {
          case (_, list) if list.size == 3 => list
        }
        .flatMap {
          case List(card1, card2, card3) =>
            groupByBestRank(bunch(board, hand) diff List(card1, card2, card3), 2)
              .collectFirst {
                case (_, List(card4, card5)) => new FullHouse(card1, card2, card3, card4, card5)
              }
        }
  }

  case class Flush(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combo {
    override val rank = 6

    override def compare(that: Combo): Int =
      that match {
        case f: Flush =>
          compareListCards(List(card1, card2, card3, card4, card5), List(f.card1, f.card2, f.card3, f.card4, f.card5))
        case _ => rank.compare(that.rank)
      }
  }

  object Flush {
    def apply(board: Board, hand: Hand): Option[Flush] =
      bunch(board, hand).groupBy(_.suit).collectFirst {
        case (_, list) if list.size >= 5 => list.sorted.reverse.take(5)
      }
        .map {
          cards => new Flush(cards.head, cards(1), cards(2), cards(3), cards(4))
        }
  }

  case class Straight(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combo {
    val isA2345 = isLowStraight(card1, card2, card3, card4, card4)
    override val rank = 5

    override def compare(that: Combo): Int =
      that match {
        case s: Straight =>
          if (isA2345 && s.isA2345) 0
          else if (isA2345 && !s.isA2345) -1
          else if (!isA2345 && s.isA2345) 1
          else
            compareListCards(List(card1, card2, card3, card4, card5), List(s.card1, s.card2, s.card3, s.card4, s.card5))
        case _ => rank.compare(that.rank)
      }
  }

  object Straight {
    def apply(board: Board, hand: Hand): Option[Straight] = {
      findBest5StraightRow(bunch(board, hand))
        .collect {
          case List(card1, card2, card3, card4, card5) =>
            new Straight(card1, card2, card3, card4, card5)
        }
    }
  }


  case class Three(card1Three: Card, card2Three: Card, card3Three: Card, kicker1: Card, kicker2: Card) extends Combo {
    override val rank = 4

    override def compare(that: Combo) =
      that match {
        case t: Three =>
          val result = card1Three.compare(t.card1Three)
          if (result == 0) compareListCards(List(kicker1, kicker2), List(t.kicker1, t.kicker2)) else result
        case _ => rank.compare(that.rank)
      }
  }

  object Three {
    def apply(board: Board, hand: Hand): Option[Three] = {
      groupByBestRank(bunch(board, hand), 3)
        .collectFirst {
          case (_, List(card1, card2, card3)) =>
            val List(kicker1, kicker2) = findKickers(List(card1, card2, card3), board, hand, 2)
            new Three(card1, card2, card3, kicker1, kicker2)
        }
    }
  }

  case class TwoPair(card1Pair1: Card, card2Pair1: Card, card3Pair2: Card, card4Pair2: Card, kicker1: Card) extends Combo {
    override val rank = 3

    override def compare(that: Combo) =
      that match {
        case tp: TwoPair =>
          val result = compareListCards(List(card1Pair1, card3Pair2), List(tp.card1Pair1, tp.card3Pair2))
          if (result == 0) kicker1.compare(tp.kicker1) else result
        case _ => rank.compare(that.rank)
      }
  }

  object TwoPair {
    def apply(board: Board, hand: Hand): Option[TwoPair] = {
      val forHand = groupByBestRank(bunch(board, hand), 2, 2)
      if (forHand.size != 2) None
      else {
        val firstPairs = forHand.head._2
        val secondPairs = forHand(1)._2
        val List(kicker) = findKickers(firstPairs ::: secondPairs, board, hand, 1)
        Some(new TwoPair(firstPairs.head, firstPairs(1), secondPairs.head, secondPairs(1), kicker))
      }
    }
  }

  case class Pair(card1Pair: Card, card2Pair: Card, kicker1: Card, kicker2: Card, kicker3: Card) extends Combo {
    override val rank = 2

    override def compare(that: Combo) =
      that match {
        case p: Pair =>
          val result = card1Pair.compare(p.card1Pair)
          if (result == 0) compareListCards(List(kicker1, kicker2, kicker3), List(p.kicker1, p.kicker2, p.kicker3)) else result
        case _ => rank.compare(that.rank)
      }
  }

  object Pair {
    def apply(board: Board, hand: Hand): Option[Pair] = {
      groupByBestRank(bunch(board, hand), 2)
        .collectFirst {
          case (_, List(card1, card2)) =>
            val List(kicker1, kicker2, kicker3) = findKickers(List(card1, card2), board, hand, 3)
            new Pair(card1, card2, kicker1, kicker2, kicker3)
        }
    }
  }

  case class High(kicker1: Card, kicker2: Card, kicker3: Card, kicker4: Card, kicker5: Card) extends Combo {
    override val rank = 1

    override def compare(that: Combo) =
      that match {
        case h: High => compareListCards(List(kicker1, kicker2, kicker3, kicker4, kicker5),
          List(h.kicker1, h.kicker2, h.kicker3, h.kicker4, h.kicker5))
        case _ => -1
      }
  }

  object High {
    def apply(board: Board, hand: Hand): High = {
      val result = bunch(board, hand).sorted.takeRight(5)
      new High(result.head, result(1), result(2), result(3), result(4))
    }
  }

  def bunch(board: Board, hand: Hand): List[Card] =
    List(board.card1, board.card2, board.card3, board.card4, board.card5, hand.card1, hand.card2)

  def groupByBestRank(list: List[Card], requiredGroupSize: Int, totalGroup: Int = 1) =
    list.groupBy(_.rank).collect {
      case (rank, list) if list.size >= requiredGroupSize => (rank, list.take(requiredGroupSize))
    }.toList.sortBy(_._1).reverse.take(totalGroup)

  def findKickers(exclude: List[Card], board: Board, hand: Hand, totalKickers: Int): List[Card] =
    (bunch(board, hand) diff exclude).sorted.reverse.take(totalKickers)

  def findBest5StraightRow(list: List[Card]): Option[List[Card]] = {
    def isStraightRow(sortedList: List[Card]) =
      sortedList.map(_.rankInt - sortedList.head.rankInt).sum == 10

    def specialCaseStraight(list: List[Card]): Option[List[Card]] = {
      val sortedCards = list.distinctBy(_.rank).sorted
      val firstCards = sortedCards.take(4)
      sortedCards.drop(4).lastOption.flatMap {
        last =>
          if ((last :: firstCards).map(_.rank).mkString("") == "A2345") Some(last :: firstCards)
          else None
      }
    }

    val distinctByRank = list.distinctBy(_.rank)
    if (distinctByRank.size < 5) None
    else
      distinctByRank.sorted.sliding(5)
        .filter(isStraightRow)
        .toList
        .lastOption
        .orElse(specialCaseStraight(list))
  }

  def isLowStraight(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) = {
    val sortedCards = List(card1, card2, card3, card4, card5).sorted
    (sortedCards.last :: sortedCards.take(4)).map(_.rank).mkString("") == "A2345"
  }

  def compareListCards(list1: List[Card], list2: List[Card]) =
    list1.sorted.zip(list2.sorted)
      .map {
        case (thisCard, thatCard) => thisCard.compare(thatCard)
      }
      .reverse
      .dropWhile(_ == 0)
      .headOption
      .getOrElse(0)
}
