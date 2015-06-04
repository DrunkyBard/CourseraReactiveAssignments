package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Var(CalculateRoots(a, b, c, delta))
  }

  private def CalculateRoots(a: Signal[Double], b: Signal[Double],
                             c: Signal[Double], delta: Signal[Double]): Set[Double] = {
    val deltaVal = delta()

    deltaVal match {
      case x if x < 0 => Set()
      case x if x == 0 => Set(-b()/(2*a()))
      case x if x > 0 => Set((-b() + scala.math.sqrt(delta()))/ (2*a()), (-b() - scala.math.sqrt(delta()))/ (2*a()))
    }
  }
}
