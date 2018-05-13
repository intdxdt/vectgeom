package vectgeom.mbr

import vectgeom.mutil._

class MBR(x1: Double, y1: Double, x2: Double, y2: Double) {
  var minx: Double = math.min(x1, x2)
  var miny: Double = math.min(y1, y2)
  var maxx: Double = math.max(x1, x2)
  var maxy: Double = math.max(y1, y2)

  def +(b: MBR): MBR =
    new MBR(
      math.min(minx, b.minx),
      math.min(miny, b.miny),
      math.max(maxx, b.maxx),
      math.max(maxy, b.maxy),
    )

  //Height of bounding box.
  def height(): Double = maxy - miny

  //Width of bounding box.
  def width(): Double = maxx - minx

  //Area of bounding box
  def area(): Double = height() * width()

  override def clone(): MBR = new MBR(minx, miny, maxx, maxy)

  //Bounding box as a closed polygon array.
  def as_poly_array(): Array[Array[Double]] = {
    Array(
      Array(minx, miny), Array(minx, maxy),
      Array(maxx, maxy), Array(maxx, miny),
      Array(minx, miny)
    )
  }

  //Lower left and upper right corners as a tuple (minx,miny, maxx,maxy)
  def as_tuple(): (Double, Double, Double, Double) =
    (minx, miny, maxx, maxy)

  //lower left and upper right as tuple ((minx,miny),(maxx,maxy))
  def llur(): ((Double, Double), (Double, Double)) =
    ((minx, miny), (maxx, maxy))

  //Compare equality of two bounding boxes are
  def equals(other: MBR): Boolean =
    feq(maxx, other.maxx) &&
      feq(maxy, other.maxy) &&
      feq(minx, other.minx) &&
      feq(miny, other.miny)

  //Checks if bounding box can be represented as a point, has area of 0.
  def is_point(): Boolean =
    feq(height(), 0.0) && feq(width(), 0.0)

  //Contains bonding box
  //is true if mbr completely contains other, boundaries may touch
  def contains(other: MBR): Boolean = {
    (other.minx >= minx) &&
      (other.miny >= miny) &&
      (other.maxx <= maxx) &&
      (other.maxy <= maxy)
  }

  //contains x, y
  def contains(x: Double, y: Double): Boolean =
    (x >= minx) &&
      (x <= maxx) &&
      (y >= miny) &&
      (y <= maxy)

  //Completely contains bonding box
  //is true if mbr completely contains other without touching boundaries
  def completely_contains(other: MBR): Boolean =
    (other.minx > minx) &&
      (other.miny > miny) &&
      (other.maxx < maxx) &&
      (other.maxy < maxy)

  //completely_contains_xy is true if mbr completely contains location with {x, y}
  //without touching boundaries
  def completely_contains(x: Double, y: Double): Boolean =
    (x > minx) &&
      (x < maxx) &&
      (y > miny) &&
      (y < maxy)

  //Translate bounding box by change in dx and dy.
  def translate(dx: Double, dy: Double): MBR =
    MBR(minx + dx, miny + dy, maxx + dx, maxy + dy)

  //Computes the center of minimum bounding box - (x, y)
  def center(): (Double, Double) =
    ((minx + maxx) / 2.0, (miny + maxy) / 2.0)


  //Checks if bounding box intersects other
  def intersects(other: MBR): Boolean =
  //not disjoint
    !(other.minx > maxx ||
      other.maxx < minx ||
      other.miny > maxy ||
      other.maxy < miny)

  //intersects point
  def intersects(pt: (Double, Double)): Boolean =
    contains(pt._1, pt._2)


  ///Computes the intersection of two bounding box
  def intersection(other: MBR): Option[MBR] = {
    if (!intersects(other)) {
      return None
    }
    val minx_ = if (minx > other.minx) minx else other.minx
    val miny_ = if (miny > other.miny) miny else other.miny
    val maxx_ = if (maxx < other.maxx) maxx else other.maxx
    val maxy_ = if (maxy < other.maxy) maxy else other.maxy

    Some(MBR(minx_, miny_, maxx_, maxy_))
  }

  //Test for disjoint between two mbrs
  def disjoint(m: MBR): Boolean = !intersects(m)


  def wkt(): String = {
    val lx = minx
    val ly = miny
    val ux = maxx
    val uy = maxy
    s"POLYGON (($lx $ly,$lx $uy,$ux $uy,$ux $ly,$lx $ly))"
  }

  //Expand include other bounding box
  def expand_by_mbr(other: MBR): MBR = {
    minx = if (other.minx < minx) other.minx else minx
    maxx = if (other.maxx > maxx) other.maxx else maxx

    miny = if (other.miny < miny) other.miny else miny
    maxy = if (other.maxy > maxy) other.maxy else maxy
    this
  }

  //Expand by delta in x and y
  def expand_by_delta(dx: Double, dy: Double): MBR = {
    val (minx_, miny_) = (minx - dx, miny - dy)
    val (maxx_, maxy_) = (maxx + dx, maxy + dy)

    minx = math.min(minx_, maxx_)
    miny = math.min(miny_, maxy_)
    maxx = math.max(minx_, maxx_)
    maxy = math.max(miny_, maxy_)

    this
  }

  //Expand to include x,y
  def expand_by_xy(x: Double, y: Double): MBR = {
    if (x < minx) {
      minx = x
    } else if (x > maxx) {
      maxx = x
    }

    if (y < miny) {
      miny = y
    } else if (y > maxy) {
      maxy = y
    }
    this
  }

  //computes dx and dy for computing hypot
  def _distance_dxdy(other: MBR): (Double, Double) = {
    var dx = 0.0
    var dy = 0.0

    // find closest edge by x
    if (maxx < other.minx) {
      dx = other.minx - maxx
    } else if (minx > other.maxx) {
      dx = minx - other.maxx
    }

    // find closest edge by y
    if (maxy < other.miny) {
      dy = other.miny - maxy
    } else if (miny > other.maxy) {
      dy = miny - other.maxy
    }

    (dx, dy)
  }


  //distance computes the distance between two mbrs
  def distance(other: MBR): Double = {
    if (intersects(other)) {
      return 0.0
    }
    val (dx, dy) = _distance_dxdy(other)
    math.hypot(dx, dy)
  }

  //distance square computes the squared distance
  //between bounding boxes
  def distance_square(other: MBR): Double = {
    if (intersects(other)) {
      return 0.0
    }
    val (dx, dy) = _distance_dxdy(other)
    (dx * dx) + (dy * dy)
  }
}

object MBR {
  def apply(x1: Double, y1: Double, x2: Double, y2: Double): MBR =
    new MBR(x1, y1, x2, y2)

  def new_raw(minx: Double, miny: Double, maxx: Double, maxy: Double): MBR = {
    val box = new MBR(0, 0, 0, 0)
    box.minx = minx
    box.miny = miny
    box.maxx = maxx
    box.maxy = maxy
    box
  }
}
