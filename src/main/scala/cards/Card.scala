package org.audreyseo.cardweave
package cards

import scala.collection.immutable.HashMap
import scala.io.AnsiColor

sealed abstract class ThreadColor(color: String, colorShort: String) {
  override def toString = colorShort
}

object ThreadColor {
  final case object White extends ThreadColor("white", s"${AnsiColor.WHITE_B}w${AnsiColor.RESET}")
  final case object Black extends ThreadColor("black", s"${AnsiColor.BLACK_B}b${AnsiColor.RESET}")
  final case object Red extends ThreadColor("red", s"${AnsiColor.RED_B}r${AnsiColor.RESET}")
  final case object Pink extends ThreadColor("pink", s"${AnsiColor.MAGENTA_B}p${AnsiColor.RESET}")
}

sealed abstract class Twist(direction: String) {
  override def toString = direction
  def flip: Twist
}

object Twist {
  final case object S extends Twist("S") {
    override def flip: Twist = Z
  }
  final case object Z extends Twist("Z") {
    override def flip: Twist = S
  }
}

case class CardRotationException(message: String) extends Exception(message)

/**
  * Any card weaving card will have a number of holes, will have a number in the lineup, an initial twist direction, and also a series of colors to be applied to the holes
  * @param numHoles the number of holes in the card
  * @param num      the order of the card in the pattern (from left to right)
  * @param t        the twist direction of the card
  * @param tcolors  all of the colors to be assigned to the card
  */
abstract class AbstractCard(numHoles: Int, num: Int, t: Twist, tcolors: ThreadColor*) {
  assert(numHoles <= tcolors.length, s"Abstract card number of holes ${numHoles} should be <= length of colors ${tcolors}")
  var positions = List("A", "B", "C", "D", "E", "F", "G", "H").slice(0, numHoles)
  var positionsToColor = HashMap(positions.zip(tcolors):_*)
  var twist = t
  var builtUpTwist = 0

  private def flip[T](l: List[T]): List[T] = {
    l.reverse
  }

  def getTwist: Twist = twist

  def getBuiltUpTwist: Int = builtUpTwist

  def flip() : this.type = {
    this.positions = flip(this.positions)
    this.twist = this.twist.flip
    this
  }

  def headPos: String = this.positions match {
    case hd :: _ => hd
    case _ => throw new Exception("Oh no")
  }

  private def rotateBackward[T](l: List[T]): List[T] = {
    l match {
      case hd :: rst =>
        rst ++ List(hd)
      case _ => throw CardRotationException(s"List ${l} was empty, but rotateForward requires a nonempty list")
    }
  }

  private def rotateForward[T](l: List[T]): List[T] = {
    (l.take(numHoles - 1), l.lastOption) match {
      case (firstThree, Some(lst)) =>
        lst :: firstThree
      case _ => throw CardRotationException(s"List ${l} didn't have a last or a first 3, but rotateBackward requires a nonempty list")
    }
  }

  def turnForward(): this.type = {
    this.positions = rotateForward(this.positions)
    this.builtUpTwist += 1
    this
  }

  def turnBackward(): this.type = {
    this.positions = rotateBackward(this.positions)
    this.builtUpTwist -= 1
    this
  }

  def getNum: Int = num

  private def positionsAndColorsString: String = {
    this.positionsToColor.toList.foldLeft("")((acc: String, tup: (String, ThreadColor)) =>
                                                tup match {
                                                  case (s, c) =>
                                                    (if (acc.isEmpty) {
                                                      ""
                                                    } else {
                                                      acc + " "
                                                    }) + s"$s:$c"

                                                })
  }

  def apply(inst: Instruction): this.type = {
    inst match {
      case Instruction.Border(_) =>
        turnForward()
      case Instruction.F(_) =>
        turnForward()
      case Instruction.B(_) =>
        turnBackward()
    }
    this
  }

  override def equals(obj: Any) = {
    obj match {
      case other: AbstractCard =>
        if (other.twist.equals(this.twist)) {
          var otherPositions = other.positions
          var index = 0
          while (!otherPositions.equals(this.positions) && index < numHoles) {
            otherPositions = rotateForward(otherPositions)
            index += 1
          }
          if (otherPositions.equals(this.positions)) {
            positions.map(pos => positionsToColor.get(pos)).equals(positions.map(pos => other.positionsToColor.get(pos)))
          } else {
            false
          }

        } else {
          false
        }
      case _ =>
        false
    }
  }


  def headColor: ThreadColor = this.positionsToColor.get(headPos) match {
    case Some(hd) => hd
    case _ => throw new Exception("Oh no")
  }

  def lastColor: ThreadColor = this.positionsToColor.get(lastPos) match {
    case Some(c) => c
    case None => throw new Exception("Oh no")
  }

  def lastPos: String = this.positions.last

  def backAtBeginning: Boolean = headPos.equals("A") && getTwist.equals(t)

  override def toString = s"Card(${getNum}${getTwist} - ${positionsAndColorsString})"
}

/**
  * A typical 4-hole card
  * @param num      the order of the card in the pattern (from left to right)
  * @param t        the twist direction of the card
  * @param A        the color in the A position
  * @param B        the color in the B position
  * @param C        the color in the C position
  * @param D        the color in the D position
  */
class Card(num: Int, t: Twist, A: ThreadColor, B:ThreadColor, C: ThreadColor, D: ThreadColor) extends AbstractCard(4, num, t, A, B, C, D)

class TriCard(num: Int, t: Twist, A: ThreadColor, B: ThreadColor, C: ThreadColor) extends AbstractCard(3, num, t, A, B, C)

class PentCard(num: Int, t: Twist, A: ThreadColor, B: ThreadColor, C: ThreadColor, D: ThreadColor, E: ThreadColor) extends AbstractCard(5, num, t, A, B, C, D, E)

class HexCard(num: Int, t: Twist, A: ThreadColor, B: ThreadColor, C: ThreadColor, D: ThreadColor, E: ThreadColor, F: ThreadColor) extends AbstractCard(6, num, t, A, B, C, D, E, F)


object Card {

  def backgroundColorCardFactory(t: ThreadColor) = new {
    var num = 0
    def apply(tw: Twist, a: ThreadColor = t, b: ThreadColor = t, c: ThreadColor = t, d: ThreadColor = t) = {
      num += 1
      new Card(num, tw, a, b, c, d)
    }

    //def apply(num: Int, tw: Twist, a: ThreadColor = t
    //          , b: ThreadColor = t, c: ThreadColor = t, d: ThreadColor = t) =
    //  new Card(num, tw, a, b, c, d)
  }

  def main(args: Array[String]) : Unit  = {
    import cards.ThreadColor._
    val c = new Card(1, Twist.S, White, Black, White, Black)
    println(c)
    println(c.turnBackward())
    println(c.turnBackward())
    println(c.turnForward())
    println(c.flip())
    println(c.turnForward())
    println(c.turnForward())
    println(c.turnForward())
    println(c.turnForward())
    println(c.flip())

    val bgWhiteCards = backgroundColorCardFactory(White)
    val c2 = bgWhiteCards(Twist.S, a=Black, d=Black)
    println(c2)

    val c3 = bgWhiteCards(Twist.S, a=Black, d=Black)

    println(c2.turnBackward().flip().turnBackward())
    println(c3.turnBackward().turnBackward().flip())

    println(c2.equals(c3))
    val c4 = bgWhiteCards(Twist.Z, a=Black, d=Black)
    println(c2.equals(c4))
    println(c4)
    println(c4.flip())
  }
}
