import scalaz._

package object cilib {
  import language.higherKinds
  import scalaz.Tag

  // Type aliases
  type RNGState[+A] = State[RNG, A]

  // scala> def test(y: Double) = StateT[Rand, RNG, Double] { rng =>
  //   | State { rng2 => { (rng, nextDouble(rng2)) }}}
  // test: (y: Double)scalaz.StateT[cilib.Rand,cilib.RNG,Double]

  type AS[F[+_], A] = StateT[F, PSO.AState, A]
  type AlgState[A] = AS[RNGState, A]

  type Solution = List[Double]
  type Velocity = List[Double]
  type Neighbours = List[Entity.Particle]
  type Fit = Option[Double]

  val Solution = List
  val Velocity = List
  val Fit = scala.Option

  val AlgState = scalaz.IndexedStateT.stateTMonadState[PSO.AState, RNGState]

  // scala> def rhoUpdate: AlgState[Int] = StateT[Rand, AState, Int] { astate =>
  //   | State[RNG, (AState, Int)] { rng => (rng, (astate, 4)) }}
  // rhoUpdate: AlgState[Int]

  // Define some un-boxed tag unions
  trait Positive
  trait Negative

  def positive(d: Double) =
    if (d > 0.0) Some(Tag[Double, Positive](d))
    else None

  def negative(d: Double) =
    if (d < 0.0) Some(Tag[Double, Negative](d))
    else None

  /**
   * Additional methods for Vectors to enable vector math operations
   */
  implicit final class VectorMathOps(val s: List[Double]) extends AnyVal {
    def +(other: List[Double]) = (s, other).zipped map { _ + _ }
    def -(other: List[Double]) = (s, other).zipped map { _ - _ }
    // ???? The two methods below are suspect to me
    def /(v: Double) = s.map(_ / v)
    def *(other: List[Double]) = (s, other).zipped map { _ * _ }
  }

}
