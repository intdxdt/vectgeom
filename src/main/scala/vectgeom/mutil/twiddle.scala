package vectgeom.mutil
/**
  * Bit twiddling hacks.
  * Based on Stanford bit twiddling hack library
  * http//graphics.stanford.edu/~seander/bithacks.html
*/


object twiddle {
  val IntBits: Int = 32 //number of bits in an integer
  val IntMax: Int = (1 << 31) - 1
  val IntMin: Int = -1 << 31

  var ReverseTable: Array[Int] = Array[Int](
    0, 128, 64, 192, 32, 160, 96, 224, 16, 144, 80, 208, 48,
    176, 112, 240, 8, 136, 72, 200, 40, 168, 104, 232, 24, 152, 88, 216, 56, 184, 120, 248, 4,
    132, 68, 196, 36, 164, 100, 228, 20, 148, 84, 212, 52, 180, 116, 244, 12, 140, 76, 204, 44,
    172, 108, 236, 28, 156, 92, 220, 60, 188, 124, 252, 2, 130, 66, 194, 34, 162, 98, 226, 18,
    146, 82, 210, 50, 178, 114, 242, 10, 138, 74, 202, 42, 170, 106, 234, 26, 154, 90, 218, 58,
    186, 122, 250, 6, 134, 70, 198, 38, 166, 102, 230, 22, 150, 86, 214, 54, 182, 118, 246, 14,
    142, 78, 206, 46, 174, 110, 238, 30, 158, 94, 222, 62, 190, 126, 254, 1, 129, 65, 193, 33,
    161, 97, 225, 17, 145, 81, 209, 49, 177, 113, 241, 9, 137, 73, 201, 41, 169, 105, 233, 25,
    153, 89, 217, 57, 185, 121, 249, 5, 133, 69, 197, 37, 165, 101, 229, 21, 149, 85, 213, 53,
    181, 117, 245, 13, 141, 77, 205, 45, 173, 109, 237, 29, 157, 93, 221, 61, 189, 125, 253, 3,
    131, 67, 195, 35, 163, 99, 227, 19, 147, 83, 211, 51, 179, 115, 243, 11, 139, 75, 203, 43,
    171, 107, 235, 27, 155, 91, 219, 59, 187, 123, 251, 7, 135, 71, 199, 39, 167, 103, 231, 23,
    151, 87, 215, 55, 183, 119, 247, 15, 143, 79, 207, 47, 175, 111, 239, 31, 159, 95, 223, 63,
    191, 127, 255)

  //not ~
  def not(v: Int): Int = -(v + 1)

  //bool to int32
  def bint32(b: Boolean): Int = if (b) 1 else 0

  //bool to uint32
  def buint32(b: Boolean): Int = if (b) 1 else 0

  //Returns -1, 0, +1 depending on sign of x
  def sign(v: Int): Int = bint32(v > 0) - bint32(v < 0)

  //Computes absolute value of integer
  def abs(v: Int): Int = {
    val mask = v >> IntBits - 1
    (v ^ mask) - mask
  }

  //Computes minimum of integers x and y
  def min(x: Int, y: Int): Int = y ^ ((x ^ y) & -bint32(x < y))

  //Computes maximum of integers x and y
  def max(x: Int, y: Int): Int = x ^ ((x ^ y) & -bint32(x < y))

  //Checks if a number is a power of two
  def isPow2(v: Int): Boolean = (v != 0) && ((v & (v - 1)) == 0)


  //Computes log base 2 of v
  def log2(o: Int): Int = {
    var v = o
    var r: Int = 0
    var shift: Int = 0
    //@formatter:off
      r     = buint32(v > 0xFFFF) << 4;   v >>= r
      shift = buint32(v > 0xFF) << 3; v >>= shift; r |= shift
      shift = buint32(v > 0xF)  << 2; v >>= shift; r |= shift
      shift = buint32(v > 0x3)  << 1; v >>= shift; r |= shift
      r | (v >> 1)
	  //@formatter:on
  }

  //Computes log base 10 of v
  def log10(v: Int): Int = {
    var r = 0
    //@formatter:off
    if (v >= 1000000000) {r = 9
    } else if (v >= 100000000) {r = 8
    } else if (v >= 10000000) {r = 7
    } else if (v >= 1000000) {r = 6
    } else if (v >= 100000) {r = 5
    } else if (v >= 10000) {r = 4
    } else if (v >= 1000) {r = 3
    } else if (v >= 100) {r = 2
    } else if (v >= 10) {r = 1}
    r
	//@formatter:on
  }

  //Counts number of bits
  def popCount(v: Int): Int = {
    //    other solution
    //    var  v = v - ((v >> 1) & 0x55555555)
    //    v = (v & 0x33333333) + ((v >> 2) & 0x33333333)
    //    ((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24 //int32 & uint32 cause overflow on this line
    var c: Int = 0
    val (s0, s1, s2, s3, s4) = (1, 2, 4, 8, 16) // magic binary numbers
    val (b0, b1, b2, b3, b4) = (0x55555555, 0x33333333, 0x0F0F0F0F, 0x00FF00FF, 0x0000FFFF)

    c = v - ((v >> s0) & b0)
    c = ((c >> s1) & b1) + (c & b1)
    c = ((c >> s2) + c) & b2
    c = ((c >> s3) + c) & b3
    ((c >> s4) + c) & b4
  }

  //Computes the number of trailing zeros by accumulating c
  // in a manner akin to binary search
  def countTrailingZeros(o: Int): Int = {
    var v = o
    var c: Int = 32
    // NOTE if 0 == v, then c = 31.
    //short circuit v == 0 as 32
    if (v == 0) {
      return c
    }

    if ((v & 0x1) != 0) {
      c = 0 // special case for odd v (assumed to happen half of the time)
    }
    else {
      c = 1
      if ((v & 0xffff) == 0) {
        v >>= 16
        c += 16
      }
      if ((v & 0xff) == 0) {
        v >>= 8
        c += 8
      }
      if ((v & 0xf) == 0) {
        v >>= 4
        c += 4
      }
      if ((v & 0x3) == 0) {
        v >>= 2
        c += 2
      }
      c -= v & 0x1
    }
    c
  }


  //Rounds to next power of 2
  def nextPow2(o: Int): Int = {
    var v = o
    v += bint32(v == 0)
    v -= 1
    v |= v >> 1
    v |= v >> 2
    v |= v >> 4
    v |= v >> 8
    v |= v >> 16
    v + 1
  }

  //Rounds down to previous power of 2
  def prevPow2(o: Int): Int = {
    var v = o
    v |= v >> 1
    v |= v >> 2
    v |= v >> 4
    v |= v >> 8
    v |= v >> 16
    v - (v >> 1)
  }

  //Computes parity of word
  def parity(o: Int): Int = {
    var v = o
    v ^= v >> 16
    v ^= v >> 8
    v ^= v >> 4
    v &= 0xf
    (0x6996 >> v) & 1
  }

  //Reverse bits in a 32 bit word
  def reverse(v: Int): Int = {
    //@formatter:off
      (ReverseTable( v&0xff)        << 24) |
      (ReverseTable((v>>8)&0xff )   << 16) |
      (ReverseTable((v>>16)&0xff )  << 8)  |
      (ReverseTable((v>>24)&0xff ))
	  //@formatter:on
  }

  //Interleave bits of 2 coordinates with 16
  // Useful for fast quadtree codes
  def interleave2(xv: Int, yv: Int): Int = {
    var x = xv
    var y = yv
    x &= 0xFFFF
    x = (x | (x << 8)) & 0x00FF00FF
    x = (x | (x << 4)) & 0x0F0F0F0F
    x = (x | (x << 2)) & 0x33333333
    x = (x | (x << 1)) & 0x55555555

    y &= 0xFFFF
    y = (y | (y << 8)) & 0x00FF00FF
    y = (y | (y << 4)) & 0x0F0F0F0F
    y = (y | (y << 2)) & 0x33333333
    y = (y | (y << 1)) & 0x55555555

    x | (y << 1)
  }


  //Extracts the nth interleaved component
  def deinterleave2(value: Int, n: Int): Int = {
    var v = value
    //@formatter:off
      v = (v >> n) & 0x55555555
      v = (v | (v >> 1)) & 0x33333333
      v = (v | (v >> 2)) & 0x0F0F0F0F
      v = (v | (v >> 4)) & 0x00FF00FF
      v = (v | (v >> 16)) & 0x000FFFF
      (v << 16) >> 16
	  //@formatter:on
  }

  //Interleave bits of 3 coordinates, each with 10
  // Useful for fast octree codes
  def interleave3(xval: Long, yval: Long, zval: Long): Long = {
    var x = xval
    var y = yval
    var z = zval

    x &= 0x3FF
    x = (x | (x << 16)) & 4278190335L
    x = (x | (x << 8)) & 251719695L
    x = (x | (x << 4)) & 3272356035L
    x = (x | (x << 2)) & 1227133513L

    y &= 0x3FF
    y = (y | (y << 16)) & 4278190335L
    y = (y | (y << 8)) & 251719695L
    y = (y | (y << 4)) & 3272356035L
    y = (y | (y << 2)) & 1227133513L
    x |= y << 1

    z &= 0x3FF
    z = (z | (z << 16)) & 4278190335L
    z = (z | (z << 8)) & 251719695L
    z = (z | (z << 4)) & 3272356035L
    z = (z | (z << 2)) & 1227133513L

    x | (z << 2)
  }

  //Extracts nth interleaved component of a 3-tuple
  def deinterleave3(value: Long, n: Long): Long = {
    var v = value
    v = (v >> n) & 1227133513L
    v = (v | (v >> 2)) & 3272356035L
    v = (v | (v >> 4)) & 251719695L
    v = (v | (v >> 8)) & 4278190335L
    v = (v | (v >> 16)) & 0x3FFL
    (v << 22) >> 22
  }

  //Computes next combination in colexicographic order (this is mistakenly called
  //nextPermutation on the bit twiddling hacks page)
  def nextCombination(value: Int): Int = {
    val v = value
    val t = v | (v - 1)
    val c = (not(t) & -not(t)) - 1
    (t + 1) | (c >> (countTrailingZeros(v) + 1))
  }
}
