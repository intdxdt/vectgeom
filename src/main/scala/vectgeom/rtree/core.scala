package vectgeom.rtree

import vectgeom.mbr.MBR

trait BoxObj {
  def BBox(): MBR
}

object core {
  def maxEntries(x: Int): Int = math.max(4, x)
}
