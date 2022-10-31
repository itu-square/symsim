package symsim

trait SymSimSpec
  extends org.typelevel.discipline.scalatest.FunSuiteDiscipline,
    org.scalatest.funsuite.AnyFunSuiteLike,
    org.scalatest.prop.Configuration:
    given PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)
