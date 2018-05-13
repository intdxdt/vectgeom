package vectgeom.mutil

import org.scalatest._


class mutilTest extends FunSpec with Matchers {
  describe("Math Util") {
    it("should test mutil") {
      (0.1 + 0.2) should not be 0.3
      feq(0.1 + 0.2, 0.3) should be(true)
      println(feq(-0.000000087422776, -0.000000087422780) )
      feq(-0.000000087422776, -0.000000087422780) should be(true)
      feq(-0.000000087422776, -0.000000087422780) should be(true)
      feq(0.0000000000000001224646799147353207, 0.0000000000000001224646799147353177) should be(true)
      feq(0.0, 0.0000000000000001224646799147353177) should be(true)
      feq(-0.0000000000000001224646799147353207, 0.0) should be(true)
      feq(-0.000000000000874227, 0.00000000000000012246) should be(true)
    }
  }
}