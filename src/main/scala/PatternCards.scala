package org.audreyseo.cardweave

import scala.::

/**
  *
  * @param cs
  */
class PatternCards(cs: Card*) {
  val cards = List(cs :_*)
  def getCards: List[Card] = cards

  override def toString =
    cards.foldLeft("")((acc: String, c: Card) => acc + s"${c.headColor}")

  def getTwists =
    cards.foldRight(List[Int]())((c: Card, acc: List[Int]) => c.getBuiltUpTwist :: acc )

  def twistsToString =
    cards.foldLeft("")((acc: String, c: Card) => if (acc.isEmpty) s"${c.getBuiltUpTwist}" else acc + s",${c.getBuiltUpTwist}")
}
