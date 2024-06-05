package org.audreyseo.cardweave

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
class Card(num: Int, t: Twist, A: ThreadColor, B:ThreadColor, C: ThreadColor, D: ThreadColor) extends AbstractCard(4, num, t, A, B, C, D) {
  var colors = List(A, B, C, D)
  //var positions = List("A", "B", "C", "D")
  var twist = t
  var builtUpTwist = 0

  def getTwist: Twist = twist

  def getBuiltUpTwist: Int = builtUpTwist

  private def flip[T](l: List[T]): List[T] = {
    l.reverse
    //l.takeRight(2).reverse ++ l.take(2).reverse
  }

  def flip() : this.type = {
    this.colors = flip(this.colors)
    this.positions = flip(this.positions)
    this.twist = this.twist.flip
    this
  }

  private def rotateBackward[T](l: List[T]): List[T] = {
    l match {
      case hd :: rst =>
        rst ++ List(hd)
      case _ => throw CardRotationException(s"List ${l} was empty, but rotateForward requires a nonempty list")
    }
  }

  private def rotateForward[T](l: List[T]): List[T] = {
    (l.take(3), l.lastOption) match {
      case (firstThree, Some(lst)) =>
        lst :: firstThree
      case _ => throw CardRotationException(s"List ${l} didn't have a last or a first 3, but rotateBackward requires a nonempty list")
    }
  }

  def headColor: ThreadColor = this.colors match {
    case hd :: _ => hd
    case _ => throw new Exception("Oh no")
  }

  def headPos: String = this.positions match {
    case hd :: _ => hd
    case _ => throw new Exception("Oh no")
  }

  override def equals(obj: Any) = {
    obj match {
      case other: Card =>
        if (other.twist.equals(this.twist)) {
          var otherPositions = other.positions
          var otherColors = other.colors
          var index = 0
          while (!otherPositions.equals(this.positions) && index < 4) {
            otherPositions = rotateForward(otherPositions)
            otherColors = rotateForward(otherColors)
            index += 1
          }
          if (otherPositions.equals(this.positions)) {
            (this.colors.equals(otherColors))
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

  def lastColor: ThreadColor = this.colors.last

  def lastPos: String = this.positions.last

  def backAtBeginning: Boolean = headPos.equals("A") && getTwist.equals(t)

  def turnForward(): this.type = {
    this.colors = rotateForward(this.colors)
    this.positions = rotateForward(this.positions)
    this.builtUpTwist += 1
    this
  }

  def turnBackward(): this.type = {
    this.colors = rotateBackward(this.colors)
    this.positions = rotateBackward(this.positions)
    this.builtUpTwist -= 1
    this
  }

  def getNum: Int = num

  private def positionsAndColorsString: String = {
    this.colors.zip(this.positions).foldLeft("")((acc: String, tup: (ThreadColor, String)) =>
                                             tup match {
                                               case (c, s) =>
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

  override def toString = s"Card(${getNum}${getTwist} - ${positionsAndColorsString})"
}


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
    import ThreadColor._
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
