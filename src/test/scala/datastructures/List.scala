import fpinscala.datastructures.List

import org.scalatest._

class ListSpec extends FlatSpec {
  import fpinscala.datastructures.List._
  "Sum test" should "15"in {
    assert(sum2( List(1,2,3,4,5)) == 15)
  }
  "product test " should " 120" in {
    assert(product2(List(1,2,3,4,5)) == 120)
  }


}