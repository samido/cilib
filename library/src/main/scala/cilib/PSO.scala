package cilib

import scala.Predef.{ any2stringadd => _, _ }
import scalaz._, Scalaz._

// trait Config {
//   def strategy: Opt
//   def w: Double
//   def c1: Double
//   def c2: Double
//   def vMax: Double
//   def k: Double
//
//   def neighbours: Option[Zipper[Entity.Particle]] => Neighbours
//
//   def evaluator: Solution => Fit
//   def interval: Interval
// }

case class Interval(val lower: Double, val upper: Double)

object PSO {
  //import AlgState.monadSyntax._

  import Entity._

  // def defaultConfig = new Config {
  //   val strategy = Min
  //   val w = 0.729844
  //   val c1 = 1.496180
  //   val c2 = 1.496180
  //   val vMax = 4.0
  //   val k = 0.5 // Number between 0 and 1 -> need a type that will enforce this
  //   def neighbours = Entity.lbest3

  //   def evaluator = s => s.map(x => x * x).sum.some
  //   val interval = Interval(-5.12, 5.12)
  // }

  trait AState {
    val w = 0.8
    val c1 = 1.4
    val c2 = 1.4
    val k = 3.0

    val interval: Interval = Interval(-5.0, 5.0)

    def strategy: Opt = Min
    def evaluator: Solution => Fit = s => s.sum.some
    def neighbours: Option[Zipper[Entity.Particle]] => Neighbours = Entity.gbest
  }

  def inertia(d: Double): AlgState[Double] =
    for {
      c <- AlgState.init // Replace with .get
    } yield c.w * d

  def randCog(d: Double): AlgState[Double] =
    for {
      c <- AlgState.init
      r <- RNG.uniform().liftM[AS]
    } yield d * r * c.c1

  def randSoc(d: Double): AlgState[Double] =
    for {
      c <- AlgState.init
      r <- RNG.uniform().liftM[AS]
    } yield d * r * c.c2

  def constriction =
    (p: Particle, n: Neighbours) => for {
      c <- AlgState.init
      nbest <- fittest(n)
      phi1 <- randCog(1.0)
      phi2 <- randSoc(1.0)
    } yield {
      val cognitive = (p.best - p.x) map { _ * phi1 }
      val social = (nbest.x - p.x) map { _ * phi2 }
      val phi = phi1 + phi2
      val d = (2 - phi - math.sqrt(phi * (phi - 4))).abs
      val X = (2 * c.k) / d
      (p.v, cognitive, social).zipped map { (x, y, z) => X * (x + y + z) }
    }

  // Create a new Velocity based on the current particle, pbest and nbest.
  def stdVelocity =
    (p: Particle, n: Neighbours) => for {
      nbest <- Entity.fittest(n)
      inertia <- p.v traverse inertia
      cognitive <- (p.best - p.x) traverse randCog
      social <- (nbest.x - p.x) traverse randSoc
    } yield (inertia, cognitive, social).zipped map { _ + _ + _ }

  def cognitive = (p: Particle, n: Neighbours) => for {
    inertia <- p.v traverse inertia
    cognitive <- (p.best - p.x) traverse randCog
  } yield (inertia, cognitive).zipped map { _ + _ }

  def social = (p: Particle, n: Neighbours) => for {
    nbest <- Entity.fittest(n)
    inertia <- p.v traverse inertia
    social <- (nbest.best - p.x) traverse randSoc
  } yield (inertia, social).zipped map { _ + _ }

  def selfless = (p: Particle, n: Neighbours) => for {
    nbest <- fittest(n filterNot { _ == p })
    inertia <- p.v traverse inertia
    social <- (nbest.best - p.x) traverse randSoc
  } yield (inertia, social).zipped map { _ + _ }

  // def barebonesVelocity: (Particle, Neighbours) => RNGStateReader[Velocity] = (p: Particle, n: Neighbours) => for {
  //   c <- ask[RNGState, Config]
  //   gbest <- fittest(n)
  //   r <- (for {
  //     sum <- (p.best + gbest.best) map { _ / 2.0 }
  //     sigma <- (p.best - gbest.best) map { _.abs }
  //   } yield gaussian(sum, sigma)).sequence.liftReaderT
  // } yield r

  // // TODO: Perhaps we need to bring in the notion of the local and global guides!
  // def fipsVelocity: (Particle, Neighbours) => RNGStateReader[Velocity] =
  //   (p: Particle, n: Neighbours) => {
  //     val len = n.length
  //     val s = n.map(y => (y.best - p.x) / len)

  //     def r(s: Solution): RNGStateReader[Vector[Double]] = for { // should be a sub for? Need to get rid of this def
  //       c <- ask[RNGState, Config]
  //       ss <- sequence(s.length)(uniform(0, c.c1 + c.c2)).liftReaderT
  //     } yield (s, ss).zipped map { _ * _ }

  //     for {
  //       c <- ask[RNGState, Config]
  //       x <- s traverse r
  //     } yield (p.v + x.reduceLeft(_ + _)).map(_ * c.w) // sum up all the neighbourhood solutions, add to the current velocity, scaled by c.w
  //   }

  /** Based on the calculated velocity, update the particle */
  def particleUpdate(f: (Solution, Velocity) => Solution) = (v: Velocity, strategy: Opt, eval: Solution => Fit) =>
    for {
      oldFit <- fitnessL
      p2 <- velocityL := v
      p3 <- positionL %= { x => f(x, p2) }
      p4 <- fitnessL := eval(p3)
      p5 <- bestPosL %= { x => if (strategy.better(p4, oldFit) === p4) p3 else x }
    } yield p5

  /** Update the particle's position using the current velocity and previous position */
  def stdParticleUpdate = particleUpdate((x: Solution, y: Velocity) => x + y)

  // /** Replace the particle's position with the given velocity value. */
  // def replacementUpdate = particleUpdate((x: Solution, y: Velocity) => y)

  // def clamp(v: (Particle, Neighbours) => RNGStateReader[Velocity]): (Particle, Neighbours) => RNGStateReader[Velocity] =
  //   (p: Particle, n: Neighbours) => for {
  //     c <- ask[RNGState, Config]
  //     vel <- v(p, n)
  //   } yield vel map { x => c.vMax min (-c.vMax max x) }

  type VelUpdate = (Particle, Neighbours) => AlgState[Velocity]

  def standardPSO = mkPSO(stdVelocity, stdParticleUpdate)

  // def cognitiveOnlyPSO = mkPSO(cognitive, stdParticleUpdate)

  // def socialOnlyPSO = mkPSO(social, stdParticleUpdate)

  // def selflessPSO = mkPSO(selfless, stdParticleUpdate)

  // def barebonesPSO = mkPSO(barebonesVelocity, replacementUpdate)

  // def fipsPSO = mkPSO(fipsVelocity, stdParticleUpdate)

  def mkPSO(velUpdate: VelUpdate, posUpdate: (Velocity, Opt, Solution => Fit) => State[Particle, Solution]): (Particle, Neighbours) => AlgState[Particle] =
    (p: Particle, n: Neighbours) => for {
      c <- AlgState.init
      v <- velUpdate(p, n)
    } yield posUpdate(v, c.strategy, c.evaluator) exec p

  type PSO = (Particle, Neighbours) => AlgState[Particle]

  /** Run the algorithm for a single, synchronous iteration */
  def syncIter(alg: PSO): (AState, List[Particle]) => AlgState[List[Particle]] = (c, xs) => {
    val z = xs.toZipper

    // def u(p: Particle) = for {
    //   s <- alg.apply(p, c.neighbours(z))
    // } yield s

    //xs.traverse(x => alg(x, c.neighbours(z)))
    xs.traverse(x => alg(x, c.neighbours(z))) // run c
  }

  // /*
  //  * Reference: https://groups.google.com/d/msg/scala-user/zG76YxLhiIE/XjVh6YaZPUkJ
  //  *
  //  * a.foldRightM[({type λ[α]=State[TreeSet[A], α]})#λ, List[A]](Nil) {
  //  *   case (a, s) =>
  //  *   State(z => if(s contains a) (s, z) else (a::s, z insert a))
  //  * } eval TreeSet.empty
  //  */
  // /** Run the algorithm for a single, asynchronous iteration */
  // def asyncIter(alg: PSO): (Config, List[Particle]) => RNGState[List[Particle]] = (c, xs) =>
  //   xs.foldLeftM[RNGState, Option[Zipper[Particle]]](xs.toZipper) {
  //     case (a, s) => State(rng => (for {
  //       zipper <- a
  //     } yield {
  //       val (rng2, newP) = alg.apply(zipper.focus, c.neighbours(a)).run(c).run(rng)
  //       (rng2, zipper.update(newP).nextC.some)
  //     }).get)
  //   }.map(x => x.map { _.toStream.toList }.getOrElse(Nil))

  /** Run the given algorithm iteration `n` times, using the config and `List[A]` */
  def runner[A](c: AState, n: Int, xs: List[A])(f: (AState, List[A]) => AlgState[List[A]]): AlgState[List[List[A]]] =
    f(c, xs) replicateM n

  def foldRun[A](c: AState, n: Int, xs: List[A])(f: (AState, List[A]) => AlgState[List[A]]): AlgState[List[A]] =
    (1 to n).toStream.foldLeftM[AlgState, List[A]](xs) {
      case (a, s) => StateT[RNGState, AState, List[A]] {
        astate => State(rng => f(astate, a).run(astate).run(rng))
      }
    }
}

object Entity {

  // case class Individual(val x: Solution, val f: Fit = None)
  case class Particle(val x: Solution, val best: Solution, val v: Velocity, val f: Fit = None)

  val positionL: Lens[Particle, Solution] = Lens.lensu((a, value) => a copy (x = value), _.x)
  val bestPosL: Lens[Particle, Solution] = Lens.lensu((a, b) => a copy (best = b), _.best)
  val velocityL: Lens[Particle, Velocity] = Lens.lensu((a, value) => a copy (v = value), _.v)
  val fitnessL: Lens[Particle, Fit] = Lens.lensu((a, value) => a copy (f = value), _.f)
  // //  val bestFitness: Lens[Particle, Fitness] = Lens.lensu((a, value) => a copy (bestf = value), _.bestf)

  def gbest: Option[Zipper[Particle]] => Neighbours = s => s.map(_.toStream.toList).getOrElse(Nil)
  // def lbest(n: Int): Option[Zipper[Particle]] => Neighbours = s => {
  //   def prev(z: Zipper[Particle]) = ((1 to n / 2) foldLeft z) { (a, _) => a.previousC }.some
  //   (s >>= { prev _ }).map(_.toStream.take(n).toList) getOrElse Nil
  // }
  // def lbest3 = lbest(3)
  // // The von neumann topology is defined such that each particle will have four neighbours:
  // // above, below and to the sides.
  // def vonNeumann: Option[Zipper[Particle]] => Neighbours = s => {
  //   def colsInRow(nRows: Int, sqSide: Int, r: Int, len: Int) = if (r === nRows - 1) len - r * sqSide else sqSide
  //   def find(t: Seq[Particle], n: Int) = (r: Int, c: Int) => t.lift(r * n + c)

  //   (for {
  //     zipper <- s
  //   } yield {
  //     val l = zipper.toIndexedSeq
  //     val sqSide = math.round(math.sqrt(l.size)).toInt
  //     val nRows = math.ceil(l.size / sqSide.toDouble).toInt
  //     val index = l.indexOf(zipper.focus)
  //     val row = (index / nRows).toInt
  //     val col = (index % nRows)
  //     val f = find(l, sqSide)

  //     val rowCols = colsInRow(nRows, sqSide, row, l.size)
  //     val north = f((row - 1 + nRows) % nRows, col)
  //     val south = f((row + 1) % nRows, col)
  //     val east = f(row, (col + 1) % rowCols)
  //     val west = f(row, (col - 1 + rowCols) % rowCols)

  //     List(zipper.focus) |+| ~(north |@| south |@| east |@| west) { List(_, _, _, _) }
  //   }).getOrElse(Nil)
  // }

  def fittest(xs: List[Particle]) =
    for {
      c <- AlgState.init
    } yield xs reduceLeft { (a, b) => if (c.strategy.better(a.f, b.f) === a.f) a else b } // Typeclass for fitness?

  def mkParticle = mkEntity((s, f) => Particle(s, s, Velocity.fill(s.length)(0.0), f))

  def mkSwarm(n: Int, d: Int) = mkParticle(d).replicateM(n)

  // import GA._
  // def mkPopulation(n: Int, d: Int) = mkIndividual(d).replicateM(n)

  // // Config lenses??
  // val intervalLowL: Lens[Interval, Double] = Lens.lensu((a, value) => a.copy(lower = value), _.lower)

  def mkEntity[A](f: (Solution, Fit) => A) = (n: Int) => for {
    c <- AlgState.init
    pos <- RNG.sequence(n)(RNG.uniform(c.interval.lower, c.interval.upper)).liftM[AS]
  } yield {
    val position = Solution(pos: _*)
    f(position, c.evaluator(position))
  }
}
