package org.audreyseo.cardweave

import Pick.pick

import org.audreyseo.cardweave.cards.{Card, Twist}
import org.audreyseo.cardweave.cards.Card.backgroundColorCardFactory

/**
  * A pattern is a series of picks
  * @param patternPicks all of the picks of the pattern, in order
  */
class Pattern(patternPicks: AbstractPick*) extends Iterable[AbstractPick] {
  var picks = List(patternPicks :_*)
  var setupPick: List[AbstractPick] = List()


  def apply(cards: Seq[Card]): Unit = {
    for (p <- picks) {
      Pick.pick(p, cards)
    }
  }



  def setSetupPicks(setupPicks: AbstractPick*): Unit = {
    this.setupPick = List(setupPicks :_*)
  }

  def setup(cards: Seq[Card]): this.type = {
    for (p <- this.setupPick) {
      Pick.pick(p, cards)
    }
    this
  }


  override def iterator: Iterator[AbstractPick] =
    this.picks.iterator
}

object Pattern {
  def main(args: Array[String]): Unit = {
    import org.audreyseo.cardweave.cards.ThreadColor._
    val cardFactory = backgroundColorCardFactory(Black)

    val cards = new PatternCards(cardFactory(Twist.Z),
                                 cardFactory(Twist.Z),
                                 cardFactory(Twist.Z, b=Pink, c=Pink),
                                 cardFactory(Twist.Z, c=Pink, d=Pink),
                                 cardFactory(Twist.Z, a=Pink, d=Pink),
                                 cardFactory(Twist.Z, a=Pink, b=Pink),
                                 cardFactory(Twist.Z, b=Pink, c=Pink),
                                 cardFactory(Twist.S, a=Pink, b=Pink),
                                 cardFactory(Twist.S, a=Pink, d=Pink),
                                 cardFactory(Twist.S, c=Pink, d=Pink),
                                 cardFactory(Twist.S, b=Pink, c=Pink),
                                 cardFactory(Twist.S),
                                 cardFactory(Twist.S)
                                 )

    println(cards)
    import Instruction._

    val pattern = new Pattern(
      BorderedPick(Border(2), B(n=5), F(n=2), B(n=2)), // 1
      BorderedPick(Border(2), B(n=5), F(n=2), B(n=2)), // 2
      BorderedPick(Border(2), F(n=2), B(n=3), F(n=4)), // 3
      BorderedPick(Border(2), F(n=2), B(n=3), F(n=4)), // 4
      BorderedPick(Border(2), F(n=4), B(1), F(4)),  // 5
      BorderedPick(Border(2), F(n=4), B(1), F(4)),  // 6
      BorderedPick(Border(2), B(2), F(3), B(4)), // 7
      BorderedPick(Border(2), B(2), F(3), B(4)), // 8
      BorderedPick(Border(2), B(4), F(1), B(4)), // 9
      BorderedPick(Border(2), B(4), F(1), B(4)), // 10
      BorderedPick(Border(2), B(9)), // 11
      BorderedPick(Border(2), B(9)),
      SetupPick(13), // 13
      SetupPick(13), // 14
      SetupPick(13), // 15
      SetupPick(13), // 16
      BorderedPick(Border(2), F(5), B(2), F(2)), // 17
      BorderedPick(Border(2), F(5), B(2), F(2)), // 18
      BorderedPick(Border(2), B(9)), // 19
      BorderedPick(Border(2), B(9)) // 20
      )

    pattern.setSetupPicks(SetupPick(13),
                          SetupPick(13),
                          SetupPick(13),
                          SetupPick(13))

    println(cards.twistsToString)
    pattern.setup(cards.getCards)
    println(cards)
    pattern(cards.getCards)
    pattern(cards.getCards)
    for (p <- pattern) {
      p(cards.getCards)
      println(cards)
    }
    println(cards.twistsToString)
    //for (p <- setupPicks) {
    //  pick(p, cards.getCards)
    //  println(cards)
    //}
    //for (i <- List(1, 2)) {
    //  for (p <- picks) {
    //    pick(p, cards.getCards)
    //    println(cards)
    //  }
    //}
    //println(cards.twistsToString)
  }
}
