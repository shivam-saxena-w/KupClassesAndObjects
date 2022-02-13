import scala.sys.{error, exit}

case class Integer(value: Nat, sign: Sign) extends Nat with Sign {
  override def isZero: Boolean =
    value.isZero
  override def predecessor: Nat = {
    if value.isZero then Integer(value.successor, negative)
    else if sign.isPositive then Integer(value.predecessor, sign)
    else Integer(value.successor, negative)
  }
  override def successor: Nat = {
    if isZero then Integer(value.successor, positive)
    else if sign.isPositive then Integer(value.successor, positive)
    else Integer(value.predecessor, negative)
  }
  override def +(that: Nat): Nat = {
    if this.isZero then that
    else if sign.isPositive then this.predecessor + that.successor
    else this.successor + that.predecessor
  }


  override def -(that: Nat): Nat = {
    if that.isZero then this
    else that match {
      case Integer(value, sign) => this + Integer(value, sign.negate)
    }
  }

  override def isPositive: Boolean = sign.isPositive
  override def negate: Sign = Integer(value, sign.negate)
}

trait Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat

}


object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = error("negative number")
  def successor: Nat = new Successor(Zero)
  def +(that: Nat): Nat = that
  def -(that: Nat): Nat = if (that.isZero) Zero else error("negative number")
}


class Successor(x: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = x
  def successor: Nat = new Successor(this)
  def +(that: Nat): Nat = x + that.successor
  def -(that: Nat): Nat = if (that.isZero) this else x - that.predecessor
}

trait Sign{
  def isPositive: Boolean
  def negate: Sign
}

object positive extends Sign {
  def isPositive: Boolean = true
  def negate: Sign = negative
}

object negative extends Sign {
  override def negate: Sign = positive
  override def isPositive: Boolean = false
}
