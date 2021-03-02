package pet.scala.s99

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.annotation.tailrec

@RunWith(classOf[JUnitRunner])
class P01Suite extends AnyFunSuite {

  @tailrec
  private def last[T](elems: Seq[T]): Option[T] =
    elems match {
      case Nil => None
      case only :: Nil => Option(only)
      case head :: tail => last(tail)
    }

  test("find the last element of a non-empty list") {
    assert(last(List(1)) == Some(1))
    assert(last(List(1, 2)) == Some(2))
    assert(last(List(1, 2, 3)) == Some(3))
    assert(last(List(1, 2, 3, 4, 5, 8)) == Some(8))
  }

  test("not find the last element of an empty list") {
    assert(last(List.empty) == None)
  }

  test("find the last element of a huge list") {
    assert(last(List.fill(100000)(0) :+ 1) == Some(1))
  }

}
