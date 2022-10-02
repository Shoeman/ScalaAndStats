package com.ScalaAndStats

import scala.math.Fractional.Implicits.infixFractionalOps
import scala.math.sqrt

object Variance {

  def populationVariance[A](values: Seq[A])(implicit A: Fractional[A]): A = {

    if (values.isEmpty) {
      return A.fromInt(0)
    }
    val mean = Averages.mean(values)
    val meanDiff: A => A = value => value - mean
    val sumDiffsSquared = values.map(meanDiff).map(a => a * a).sum

    A.div(sumDiffsSquared, A.fromInt(values.size))
  }

  def populationStandardDeviation[A](values: Seq[A])(implicit A: Fractional[A]): Double = {
    val variance = populationVariance(values)
    sqrt(variance.toDouble)
  }
}
