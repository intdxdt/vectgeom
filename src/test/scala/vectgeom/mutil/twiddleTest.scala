package vectgeom.mutil

import twiddle._
import org.scalatest._

class twiddleTest extends FunSpec with Matchers {
  describe("Twiddle") {
    it("should test twiddle") {
      twiddle.not(170) should be(-171)
      twiddle.not(0) should be(-1)
      twiddle.not(-3) should be(2)

      sign(-100) should be(-1)
      sign(100) should be(1)
      sign(0) should be(0)
      sign(IntMax) should be(1)
      sign(IntMin) should be(-1)

      abs(0) should be(0)
      abs(1) should be(1)
      abs(-1) should be(1)
      abs(IntMax) should be(IntMax)
      abs(-IntMax) should be(IntMax)

      min(0, 0) should be(0)
      min(-1, 1) should be(-1)
      min(IntMax, IntMax) should be(IntMax)
      min(IntMin, IntMin) should be(IntMin)
      min(IntMax, IntMin) should be(IntMin)

      max(0, 0) should be(0)
      max(-1, 1) should be(1)
      max(IntMax, IntMax) should be(IntMax)
      max(IntMin, IntMin) should be(IntMin)
      max(IntMax, IntMin) should be(IntMax)

      isPow2(0) should be(false)
      for (i <- 0 until 31) {
        isPow2(1 << i) should be(true)
      }
      isPow2(100) should be(false)
      isPow2(0x7fffffff) should be(false)
      isPow2(-1000000) should be(false)

      for (i <- 0 until 31) {
        isPow2(1 << i) should be(true)
        if (i > 0) {
          log2((1 << i) - 1) should be(i - 1)
          log2((1 << i) + 1) should be(i)
        }
        log2(1 << i) == i
      }


      log10(1) should be(0)
      log10(10) should be(1)
      log10(100) should be(2)
      log10(1000) should be(3)
      log10(10000) should be(4)
      log10(100000) should be(5)
      log10(1234007) should be(6)
      log10(10004659) should be(7)
      log10(100046598) should be(8)
      log10(1000465983) should be(9)

      popCount(0) should be(0)
      popCount(1) should be(1)
      //g.Assert(pop_count(-1), 32)
      for (i <- 0 until 31) {
        popCount(1 << i) should be(1)
        popCount((1 << i) - 1) should be(i)
      }
      popCount(0xf0f00f0f) should be(16) //overflow for int32

      //count trailing zeros
      countTrailingZeros(0) should be(32)
      countTrailingZeros(1) should be(0)
      //    g.Assert(CountTrailingZeros(-1), 0)
      for (i <- 0 until 31) {
        countTrailingZeros(1 << i) should be(i)
        if (i > 0) {
          countTrailingZeros((1 << i) - 1) should be(0)
        }
      }
      countTrailingZeros(0xf81700) should be(8)


      //next power of 2
      for (i <- 0 until 31) {
        if (i != 1) {
          nextPow2((1 << i) - 1) should be(1 << i)
        }
        nextPow2(1 << i) should be(1 << i)
        if (i < 30) {
          nextPow2((1 << i) + 1) should be(1 << (i + 1))
        }
      }

      //prev power of 2
      print("%2s    %10s    %10s\n".format("i", "((1 << i) + 1)", "prevPow2"))
      println("-" * 34)
      for (i <- 0 until 31) {
        if (i > 0) {
          prevPow2((1 << i) - 1) should be(1 << (i - 1))
        }
        prevPow2(1 << i) should be(1 << i)

        if (0 < i && i < 30) {
          print("%2d .. %10d .. %10d\n".format(i, (1 << i) + 1, prevPow2((1 << i) + 1)))
          prevPow2((1 << i) + 1) should be(1 << i)
        }
      }

      //parity
      parity(1) should be(1)
      parity(0) should be(0)
      parity(0xf) should be(0)
      parity(0x10f) should be(1)

      //reverse
      reverse(0) should be(0)

      //next combination
      nextCombination(1) should be(2)
      nextCombination(0x300) should be(0x401)

      //interleave 2
      for (x <- 0 until 100) {
        for (y <- 0 until 100) {
          val h = interleave2(x, y)
          deinterleave2(h, 0) should be(x)
          deinterleave2(h, 1) should be(y)
        }
      }

      //interleave 3
      for (x <- 0 to 25) {
        for (y <- 0 to 25) {
          for (z <- 0 to 25) {
            val h = interleave3(x, y, z)
            deinterleave3(h, 0) should be(x)
            deinterleave3(h, 1) should be(y)
            deinterleave3(h, 2) should be(z)
          }
        }
      }

    }
  }
}