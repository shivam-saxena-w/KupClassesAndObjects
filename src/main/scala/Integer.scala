import scala.sys.error

trait Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

/*
 * The object Zero is provided by the text.
 */
object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = error("negative number")
  def successor: Nat = new Succ(Zero)
  def +(that: Nat): Nat = that
  def -(that: Nat): Nat = if (that.isZero) Zero else error("negative number")

}

/*
 * The class Succ is provided by the text.
 */
class Succ(x: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = x
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat = x + that.successor
  def -(that: Nat): Nat = if (that.isZero) this else x - that.predecessor
}

trait Sign {
  def isPositive: Boolean
  def negate: Sign
}

object Positive extends Sign {
  def isPositive: Boolean = true
  def negate: Sign = Negative
}

object Negative extends Sign {
  def isPositive: Boolean = false
  def negate: Sign = Positive
}

case class Integer(value: Nat, sign: Sign = Positive) extends Nat with Sign {

  def isZero: Boolean = value.isZero

  def predecessor: Nat =
    if (isZero) new Integer(value.successor, Negative)
    else if (sign.isPositive) new Integer(value.predecessor, sign)
    else new Integer(value.successor, Negative)

  def successor: Nat =
    if (isZero) new Integer(value.successor, Positive)
    else if (sign.isPositive) new Integer(value.successor, sign)
    else new Integer(value.predecessor, Negative)

  def +(that: Nat): Nat =
    if (isZero) that
    else if (sign.isPositive) this.predecessor + that.successor
    else this.successor + that.predecessor

  def -(that: Nat): Nat =
    if (that.isZero) this
    else that match {
      case Integer(v, s) => this + new Integer(v, s.negate)
    }

  def isPositive: Boolean = sign.isPositive

  def negate: Integer = new Integer(value, sign.negate)

}
