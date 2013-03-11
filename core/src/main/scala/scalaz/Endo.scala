package scalaz

import syntax.Ops

/** Endomorphisms have special properties among arrows, so are captured in this newtype.
  */
sealed trait Endomorphic[=>:[_, _], A] {

  implicit val F: Arrow[=>:]

  def run: A =>: A

  final def compose(that: Endomorphic[=>:, A]): Endomorphic[=>:, A] = Endomorphic(F.compose(run, that.run))

  final def andThen(that: Endomorphic[=>:, A]): Endomorphic[=>:, A] = that.compose(this)

}

trait EndoOps[A] extends Ops[Endo[A]] {

  final def apply(a: A): A = self.run(a)

  /** `run(run(run(...` */
  final def fix: A = self.run(fix)

}

object Endo extends EndoFunctions with EndoInstances {

  implicit def toEndoOps[A](endo: Endo[A]): EndoOps[A] = new EndoOps[A] {
    final def self = endo
  }

  implicit val Function1Arrow: Arrow[Function1] = scalaz.std.function.function1Instance

  def apply[A](f: A => A): Endo[A] = new Endomorphic[Function1, A] {
    implicit val F = Function1Arrow
    val run = f
  }
}

object Endomorphic extends EndomorphicFunctions with EndomorphicInstances {

  def apply[=>:[_, _], A](arr: A =>: A)(implicit F: Arrow[=>:]) = new Endomorphic[=>:, A] {
    implicit val F: Arrow[=>:] = F
    val run = arr
  }
}

trait EndomorphicFunctions {

  /** Endomorphic Kleisli arrow */
  final def endoKleisli[F[+_] : Monad, A](f: A => F[A]): Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A] =
    Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A](Kleisli(f))
}

trait EndomorphicInstances {

  implicit def endomorphicInstance[=>:[_, _], A](implicit F: Arrow[=>:]): Monoid[Endomorphic[=>:, A]] =
    new Monoid[Endomorphic[=>:, A]] {
      val mon = F.monoid[A]
      def append(f1: Endomorphic[=>:, A], f2: => Endomorphic[=>:, A]) = Endomorphic(mon.append(f1.run, f2.run))
      def zero: Endomorphic[=>:, A] = Endomorphic(mon.zero)
    }

  implicit def kleisliEndoInstance[F[+_] : Monad, A]: Monoid[Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]] =
    endomorphicInstance[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]
}

trait EndoInstances {

  implicit def endoInstances: Zip[Endo] with Unzip[Endo] = new Zip[Endo] with Unzip[Endo] {
    def zip[A, B](a: => Endo[A], b: => Endo[B]) =
      Endo {
        case (x, y) => (a.run(x), b.run(y))
      }

    // CAUTION: cheats with null
    def unzip[A, B](a: Endo[(A, B)]) =
      (Endo(x => a.run((x, null.asInstanceOf[B]))._1), Endo(x => a.run((null.asInstanceOf[A], x))._2))
  }
}

trait EndoFunctions {
  /** Alias for `Endo.apply`. */
  final def endo[A](f: A => A): Endo[A] = Endo.apply(f)

  /** Always yield `a`. */
  final def constantEndo[A](a: => A): Endo[A] = endo[A](_ => a)

  /** Alias for `Monoid[Endo[A]].zero`. */
  final def idEndo[A]: Endo[A] = endo[A](a => a)

  import Isomorphism.{IsoSet, IsoFunctorTemplate}

  implicit def IsoEndo[A] = new IsoSet[Endo[A], A => A] {
    def to: (Endo[A]) => A => A = _.run
    def from: (A => A) => Endo[A] = endo
  }

  implicit def IsoFunctorEndo = new IsoFunctorTemplate[Endo, ({type λ[α]=(α => α)})#λ] {
    def to[A](fa: Endo[A]): A => A = fa.run
    def from[A](ga: A => A): Endo[A] = endo(ga)
  }
}
