package pet.scala.s99

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class P01Suite extends AnyFunSuite {

  def last[T](elems: Seq[T]): Option[T] =
    elems match {
      case Nil => ???
      case head :: Nil => Option(head)
      case head :: tail => last(tail)
    }

  test("find the last element of a list") {
    assert(last(List(1, 2, 3, 4, 5, 8)) == Some(8))
  }
}
