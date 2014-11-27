package scalaz

import org.scalacheck.Prop.forAll

import scalaz.Free.Trampoline
import scalaz.std.option._
import scalaz.syntax.applicativePlus._

object ApplicativePlusTest extends SpecLite {

  "some" ! forAll { input: String =>
    val parser = ApplicativePlus[P].some(P.anyChar)
    val parsed = parser.parse(input)

    parsed.isEmpty must_== input.isEmpty
    parsed.toList.map(nel => nel.head :: nel.tail).flatten.mkString must_== input
  }

  "many" ! forAll { input: String =>
    val parser = ApplicativePlus[P].many(P.anyChar)
    val parsed = parser.parse(input)

    parsed.nonEmpty must_== true
    parsed.toList.flatten.mkString must_== input
  }

  "many is stack-safe if F is stack-safe" in {
    val parser = ApplicativePlus[P].many(P.anyChar)
    parser.parse(List.fill(1000)("foo").mkString)
    ()
  }

}

// A simple parser
final case class P[A](run: String => OptionT[Trampoline, (A, String)]) {
  def parse(s: String): Option[A] = run(s).map(_._1).run.run
}

object P {

  def anyChar: P[Char] =
    P(s => OptionT(
      Trampoline.done(if (s.isEmpty) None else Some((s.head, s.substring(1))))
    ))

  implicit val pMonadPlus: MonadPlus[P] =
    new MonadPlus[P] {
      def empty[A]: P[A] =
        P(_ => OptionT.none)
      def plus[A](a: P[A], b: => P[A]): P[A] =
        P(s => a.run(s) <+> b.run(s))
      def point[A](a: => A): P[A] =
        P(s => OptionT.some((a, s)))
      def bind[A, B](fa: P[A])(f: (A) => P[B]): P[B] =
        P(s => fa.run(s).flatMap { case (a, s1) => f(a).run(s1) })
    }

}
