package vectgeom.mbr

import org.scalatest._

class MBRTest extends FunSpec with Matchers {
  describe("Minimum Bounding Box") {
    it("should test mbr utils") {
      val m0 = MBR(0.0, 0.0, 0.5, 0.2)
      var m1 = MBR(2.0, 2.0, -0.5, -0.2)
      var m = m0 + m1
      (m0.minx, m0.miny, m0.maxx, m0.maxy) should be(0.0, 0.0, 0.5, 0.2)
      (m1.minx, m1.miny, m1.maxx, m1.maxy) should be(-0.5, -0.2, 2.0, 2.0)
      (m.minx, m.miny, m.maxx, m.maxy) should be(-0.5, -0.2, 2.0, 2.0)

      m1 = MBR.new_raw(2.0, 2.0, -0.5, -0.2)
      (m1.minx, m1.miny, m1.maxx, m1.maxy) should be(2.0, 2.0, -0.5, -0.2)

      m = MBR(2.0, 2.0, 0.5, 0.2)
      (m.minx, m.miny, m.maxx, m.maxy) should be(0.5, 0.2, 2.0, 2.0)

      m.height() should be(1.8)
      m.width() should be(1.5)
      m.area() should be(1.5 * 1.8)
      !m.is_point() should be(true)
      m.as_tuple() should be(0.5, 0.2, 2.0, 2.0)

      val b = m.as_poly_array()
      b.length should be(5)
      b(0)(0) should be(b(4)(0))
      b(0)(1) should be(b(4)(1))

      m1 = m.clone()
      m1.area() should be(m.area())
      0.1 + 0.2 should not be 0.3

      m1.equals(m) should be(true)

      println(m.wkt())
    }
  }
}