package cilib

import org.scalacheck._
import org.scalacheck.Prop._

object Combinators {
  @annotation.tailrec
  def until[A](p: A => Boolean)(f: A => A)(z: A): A = if (p(z)) z else until(p)(f)(f(z))
}

object GeneratorTest extends Properties("Distribution") {
  import Combinators.until

  def phi_gauss(x: Double) = math.exp(-x * x / 2) / math.sqrt(2 * math.Pi)

  def cdf_gauss = (z: Double) => {
    if (z < -8.0) 0.0
    else if (z > 8.0) 1.0
    else {
      def test = (sum: Double, term: Double, _: Int) => sum + term == sum
      def sum = (a: Double, b: Double, i: Int) => (a + b, b * z * z / i, i + 2)
      0.5 + until(test.tupled)(sum.tupled)((0.0, z, 3))._1 * phi_gauss(z)
    }
  }

  // NB: java.util.math.log returns ln !!!!
  def S(x: Array[Double], F: Double => Double) = {
    val n = x.size
    val m = x.foldLeft(0.0)(_ + _) / x.size
    val stdDev = math.sqrt((1.0 / (n - 1)) * x.foldLeft(0.0)((a, b) => a + (b - m) * (b - m)))
    val Y = x.map(a => (a - m) / stdDev).map(F).sorted

    (1.0 / n) * (1 to n).foldLeft(0.0) {
      (s, i) => s + ((2 * i - 1) * math.log(Y(i - 1)) + (2 * (n - i) + 1) * math.log(1.0 - Y(i - 1)))
    }
  }

  // Generator for gaussian numbers
  implicit val arbRandom: Arbitrary[Array[Double]] = Arbitrary[Array[Double]] {
    val gaussian = RNG.gaussian()

    Gen.sized { n => RNG.sequence(n)(gaussian).run(RNG.init())._2.toArray[Double] }
  }

  property("Gaussian hypothesis test") = forAll {
    (a: Array[Double]) =>
      (a.size >= 100 && a.size <= 1000) ==> {
        val n = a.size
        val a2 = -n - S(a, cdf_gauss)

        if (a2 < 3.857) true else false
      }
  }
}
