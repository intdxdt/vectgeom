package vectgeom.mbr

class mbr(x1: Double, y1: Double, x2: Double, y2: Double) {
  val minx: Double = math.min(x1, x2)
  val miny: Double = math.min(y1, y2)
  val maxx: Double = math.max(x1, x2)
  val maxy: Double = math.max(y1, y2)
}

object mbr {
  def apply(x1: Double, y1: Double, x2: Double, y2: Double): mbr =
    new mbr(x1, y1, x2, y2)
}
