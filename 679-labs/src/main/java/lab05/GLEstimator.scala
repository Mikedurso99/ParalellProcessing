package lab05

import scala.math.pow

/**
 * Estimates pi using the Gregory-Leibniz series
 */
object GLEstimator extends App{
  val startTime = System.nanoTime() // t0
  val pi = piInRange(0, Int.MaxValue) * 4
  val runTime = (System.nanoTime() - startTime) / 1000000000.0 // (t1 - t0)

  // Formatted print statements
  println("Pi = " + pi)
  println(f"dt = $runTime%1.2f")

  def piInRange(lower: Long, upper: Long): Double = {
    var index = lower
    var pi = 0.0
    while(index < upper) {
      val numerator = if ((index & 1) == 1) - 1 else 1
      pi += numerator / (2.0 * index + 1)
      index += 1
    }
    pi
  }
}