package scalaz

////
/**
 * [[scalaz.Applicative]] combined with [[scalaz.PlusEmpty]].
 */
////
trait ApplicativePlus[F[_]] extends Applicative[F] with PlusEmpty[F] { self =>
  ////

  /**The composition of ApplicativePlus `F` and `G`, `[x]F[G[x]]`, is a ApplicativePlus */
  def compose[G[_]](implicit G0: ApplicativePlus[G]): ApplicativePlus[({type λ[α] = F[G[α]]})#λ] = new CompositionApplicativePlus[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of ApplicativePlus `F` and `G`, `[x](F[x], G[x]])`, is a ApplicativePlus */
  def product[G[_]](implicit G0: ApplicativePlus[G]): ApplicativePlus[({type λ[α] = (F[α], G[α])})#λ] = new ProductApplicativePlus[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /** `empty` or a non-empty list of results acquired by repeating `a`. */
  def some[A](a: F[A]): F[NonEmptyList[A]] =
    apply2(a, many(a))((x, xs) => NonEmptyList.nel(x, xs.toList))

  /** A list of results acquired by repeating `a`.  Never `empty`;
    * initial failure is an empty list instead.
    */
  def many[A](a: F[A]): F[List[A]] =
    plus(apply2(a, many(a))(_ :: _), point(Nil))

  ////
  val applicativePlusSyntax = new scalaz.syntax.ApplicativePlusSyntax[F] { def F = ApplicativePlus.this }
}

object ApplicativePlus {
  @inline def apply[F[_]](implicit F: ApplicativePlus[F]): ApplicativePlus[F] = F

  ////

  ////
}
