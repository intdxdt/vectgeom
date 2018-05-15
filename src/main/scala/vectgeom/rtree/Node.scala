package vectgeom.rtree

import vectgeom.mbr.MBR
import scala.collection.mutable.ArrayBuffer

class Node() {
  var item: BoxObj
  var bbox: MBR
  var height: Int
  var leaf: Boolean
  var children: ArrayBuffer[Node] = ArrayBuffer.empty[Node]
}
