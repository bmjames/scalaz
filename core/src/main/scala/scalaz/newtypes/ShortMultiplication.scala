package scalaz
package newtypes

sealed trait ShortMultiplication {
  val value: Short
}

object ShortMultiplication extends ShortMultiplications

trait ShortMultiplications {
  implicit val ShortMultiplicationNewtype: Newtype[ShortMultiplication, Short] =
    Newtype.newtype(_.value, b => new ShortMultiplication {
      val value = b
    })

  implicit def ShortMultiplicationZero: Zero[ShortMultiplication] =
    Zero.zero(implicitly[Newtype[ShortMultiplication, Short]].pack(1))

  implicit def ShortMultiplicationSemigroup: Semigroup[ShortMultiplication] = new Semigroup[ShortMultiplication] {
    def append(a1: ShortMultiplication, a2: => ShortMultiplication) =
      implicitly[Newtype[ShortMultiplication, Short]].pack((a1.value.toInt * a2.value.toInt).toShort)
  }

  implicit def ShortMultiplicationMonoid: Monoid[ShortMultiplication] =
    Monoid.monoid

  implicit def ShortMultiplicationShow: Show[ShortMultiplication] =
    Show.UnpackShow[ShortMultiplication, Short]

  implicit def ShortMultiplicationEqual: Equal[ShortMultiplication] =
    Equal.UnpackEqual[ShortMultiplication, Short]

  implicit def ShortMultiplicationOrder: Order[ShortMultiplication] =
    Order.UnpackOrder[ShortMultiplication, Short]

}
