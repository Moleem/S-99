package pet.scala.s99.logic

class S99Logic(a: Boolean) {
  import S99Logic._

  def and(b: Boolean): Boolean = AND(a, b)
  def or(b: Boolean): Boolean = OR(a, b)
  def nand(b: Boolean): Boolean = NAND(a, b)
  def nor(b: Boolean): Boolean = NOR(a, b)
  def xor(b: Boolean): Boolean = XOR(a, b)
  def impl(b: Boolean): Boolean = IMPL(a, b)
  def equ(b: Boolean): Boolean = EQU(a, b)
}

object S99Logic {
  implicit def enrichBoolean(a: Boolean): S99Logic =
    new S99Logic(a)

  def AND(a: Boolean, b: Boolean): Boolean =
    (a, b) match {
      case (true,  true)  => true
      case _ => false
    }  

  def OR(a: Boolean, b: Boolean): Boolean =
    (a, b) match {
      case (false, false) => false
      case _ => true
    }

  def NOT(a: Boolean): Boolean =
    a match {
      case false => true
      case true  => false
    }

  def NAND(a: Boolean, b: Boolean): Boolean =
    NOT(AND(a, b))

  def NOR(a: Boolean, b: Boolean): Boolean =
    NOT(OR(a, b))

  def XOR(a: Boolean, b: Boolean): Boolean =
    AND(OR(a, b), NAND(a, b))

  def IMPL(a: Boolean, b: Boolean): Boolean =
    OR(NOT(a), b)

  def EQU(a: Boolean, b: Boolean): Boolean =
    NOT(XOR(a, b))

  def table(f: (Boolean, Boolean) => Boolean): List[List[String]] = {
    val header = List("A", "B", "Result")
    val results = for {
      a <- List(false, true);
      b <- List(false, true)
    } yield List(a.toString, b.toString, f(a, b).toString)
    header :: results
  }
}