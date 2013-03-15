package cilib

import scalaz._, Scalaz._

sealed trait Opt {
  def better(a: Fit, b: Fit) = (a, b) match {
    case (Some(_), Some(_)) => compare(a, b)
    case _ => withNoFitness(a, b)
  }

  protected def compare(a: Fit, b: Fit): Fit

  private def withNoFitness(a: Fit, b: Fit) = {
    val aa: MaxOption[Double] = Tag(a)
    val bb: MaxOption[Double] = Tag(a)
    aa |+| bb
  }
}

final case object Min extends Opt {
  def compare(a: Fit, b: Fit) = {
    val aa: MinOption[Double] = Tag(a)
    val bb: MinOption[Double] = Tag(b)
    aa |+| bb
  }
}

final case object Max extends Opt {
  def compare(a: Fit, b: Fit) = {
    val aa: MaxOption[Double] = Tag(a)
    val bb: MaxOption[Double] = Tag(b)
    aa |+| bb
  }
}
