package object symsim {

  /** Method syntax for easy conversion to Prop from Scheduled Booleans */
  implicit class CanTestInOps[F[_]: CanTestIn] (fProp: F[Boolean])  {

    def toProp: org.scalacheck.Prop =
      CanTestIn[F].test (fProp)

  }

}
