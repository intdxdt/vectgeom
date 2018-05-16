package vectgeom.rtree
import vectgeom.mbr.MBR
import scala.collection.mutable.ArrayBuffer

trait BoxObj {
  def BBox(): MBR = MBR.new_raw(0, 0, -1, -1)
}

class Node[T](obj: T with BoxObj) extends BoxObj {
  var item: T = obj
  var height: Int = 0
  var leaf: Boolean = false
  var children: ArrayBuffer[Node[T]] = ArrayBuffer.empty[Node[T]]
}

class RTree[T](cap: Int) {

}

object RTree {

}
