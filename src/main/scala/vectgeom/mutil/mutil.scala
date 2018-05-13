package vectgeom

package object mutil extends scala.AnyRef {

  val Ln2: Double = 0.693147180559945309417232121458176568075500134360255254120680009 // https://oeis.org/A002162
  val PRECISION: Double = 12
  val EPSILON: Double = 1.0e-12
  val Sqrt2: Double = 1.41421356237309504880168872420969807856967187537694807317667974 // https://oeis.org/A002193
  val Pi: Double = Math.PI
  val Tau: Double = 2.0 * Pi

  //Compares the equality of two floats @ nearest `eps = 1-e10`
  // is `a == b` ?, generally 0.3 != 0.1 + 0.2.
  //Ref: http://floating-point-gui.de/errors/comparison/
  //Ref: https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
  //assert!(0.3 != 0.1 + 0.2)
  //assert!(feq(0.3, 0.1 + 0.2))
  //:param b:
  //:param a:
  def feq_eps(a: Double, b: Double, eps: Double): Boolean = {
    val abs_a = math.abs(a)
    val abs_b = math.abs(b)
    val diff = math.abs(a - b)

    if (a == b) {
      return true
    }
    else if (a == 0.0 || b == 0.0 || diff < eps) {
      // a or b is zero or both are extremely close to it
      // relative error is less meaningful here
      return (diff < eps) || (diff < (eps * eps))
    }
    // use relative error
    (diff / (abs_a + abs_b).min(Double.MaxValue)) < eps
  }

  def feq(a:Double, b:Double):Boolean =
    feq_eps(a, b, EPSILON)
}
