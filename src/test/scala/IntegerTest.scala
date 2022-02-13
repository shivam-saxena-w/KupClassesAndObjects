import org.scalatest.funsuite.AnyFunSuite

class IntTester extends AnyFunSuite {
  val zero =  Zero
  val one =  new Successor(zero)
  val two = new Successor(one)
  val three = new Successor(two)

  test("Test one"){
    assert(one.predecessor.isZero)
  }
  test("Test two"){
    assert(one.predecessor == zero)
  }
  test("Test three"){
    assert(one.+(two).predecessor.predecessor.predecessor.isZero)
  }
  test("Test four"){
    assert(two.-(one) == one)
  }
}
