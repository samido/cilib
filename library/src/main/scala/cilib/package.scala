import scalaz._, Scalaz._

package object cilib {
  import scalaz.Tag

  // Type aliases
  type Rand[+A] = State[RNG, A]

  // Define some un-boxed tag unions
  trait Positive
  trait Negative

  def positive(d: Double) =
    if (d > 0.0) Some(Tag[Double, Positive](d))
    else None

  def negative(d: Double) =
    if (d < 0.0) Some(Tag[Double, Negative](d))
    else None
}
