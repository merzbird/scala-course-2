package calculator

import scala.math.sqrt
import scala.math.pow

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
  c: Signal[Double]): Signal[Double] = {
    Signal {
      pow(b(), 2) - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def f(a: Double, b: Double, delta: Double) = (-b + delta) / (2 * a)
    Signal {
      delta() match {
        case d if d < 0 => Set()
        case d if d == 0 => Set(-(b() / (2 * a())))
        case d if d > 0 => {
          val aVal = a()
          val bVal = b()
          Set(f(aVal, bVal, sqrt(d)), f(aVal, bVal, -sqrt(d)))
        }
      }
    }
  }
}
