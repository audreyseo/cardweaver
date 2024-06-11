package org.audreyseo.cardweave

import org.audreyseo.cardweave.cards.{Card, Twist}
import org.audreyseo.cardweave.cards.Card.backgroundColorCardFactory
import org.audreyseo.cardweave.cards.ThreadColor.White

case class PickException(message: String) extends Exception(message)

/**
  * A pick is a series of instructions for the whole set of cards
  */
abstract class AbstractPick {
  def getInstructions: Iterator[Instruction]

  def length : Int = getInstructions.foldLeft(0)((num: Int, inst: Instruction) => inst.getNumCards + num)

  def apply(cards: Seq[Card]) : this.type = {
    Pick.pick(this, cards)
    this
  }
}

class AlwaysTheSameIterator(limit: Int, i: Instruction) extends Iterator[Instruction] {
  var iter = 0
  override def hasNext: Boolean = iter == limit

  override def next(): Instruction = {
    iter += 1
    i
  }
}

case class Pick(insts: Instruction*) extends AbstractPick {
  override def getInstructions = insts.iterator
}

case class SetupPick(n: Int) extends AbstractPick {
  override def getInstructions = List(Instruction.F(n=n)).iterator // new AlwaysTheSameIterator(n, Instruction.F())

  override def length = n
}

case class BorderedPick(borderInst: Instruction.Border, middleInsts: Instruction*) extends AbstractPick {
  override def getInstructions = (borderInst :: (middleInsts.toList ++ List(borderInst) )).iterator
}

object Pick {

  def pick(p: AbstractPick, cards: Seq[Card]) : Unit = {
    assert(p.length == cards.length, s"Length of pick ${p} is not the same as ${cards}: ${p.length} vs ${cards.length}")
    val picks = p.getInstructions
    var cardIndex = 0
    while (picks.hasNext) {
      val inst = picks.next()
      for (card <- cards.slice(cardIndex, cardIndex + inst.getNumCards)) {
        card(inst)
        //println(card(inst))
      }
      cardIndex += inst.getNumCards
    }
  }

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
    val setupPicks = List(SetupPick(13),
                          SetupPick(13),
                          SetupPick(13),
                          SetupPick(13))
    val picks = List(
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

    println(cards.twistsToString)
    for (p <- setupPicks) {
      pick(p, cards.getCards)
      println(cards)
    }
    for (i <- List(1, 2)) {
      for (p <- picks) {
        pick(p, cards.getCards)
        println(cards)
      }
    }
    println(cards.twistsToString)

  }
}
