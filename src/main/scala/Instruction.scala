package org.audreyseo.cardweave

/**
  * An instruction is an action that is applied to a number of cards
  * @param numCards
  */
sealed abstract class Instruction(numCards: Int) {
  assert(numCards >= 1, s"Instruction should be initialized with n >= 1, but got ${numCards}")

  def getNumCards: Int = numCards
}

object Instruction {

  /**
    * The instruction for border cards, which are typically always turned forward
    * @param n the number of border cards to apply this to
    */
  case class Border(n: Int = 1) extends Instruction(n) {

  }

  /**
    * A forward instruction, i.e., (A B C D) -> (D A B C)
    * @param n
    */
  case class F(n: Int = 1) extends Instruction(n)

  /**
    * A backward instruction, i.e., (A B C D) -> (B C D A)
    * @param n
    */
  case class B(n: Int = 1) extends Instruction(n)
}
