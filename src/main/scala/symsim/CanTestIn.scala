package symsim

/** A simple type class that defines contexts (schedulers) in which we can test
 *  with scalacheck
 */
trait CanTestIn[F[_]] {

  def test (fProp: F[Boolean]): org.scalacheck.Prop

}

object CanTestIn {

  /** Allow to use CanTestIn[F] to access the implicit available instance of
    * CanTestIn for F.
    */
  def apply[F[_]: CanTestIn] = implicitly[CanTestIn[F]]

}
