package com.ScalaAndStats

import scala.math.Fractional.Implicits.infixFractionalOps
import scala.math.sqrt

object Variance {

  def sumVariance[A](values: Seq[A])(implicit A: Fractional[A]): A = {
    val mean = Averages.mean(values)
    val meanDiff: A => A = value => value - mean
    values.map(meanDiff).map(a => a * a).sum
  }

  def varianceWithDivisor[A](divisor: Int)(values: Seq[A])(implicit A: Fractional[A]): A = {
    val varianceSum = sumVariance(values)
    A.div(varianceSum, A.fromInt(divisor))
  }

  def populationVariance[A](values: Seq[A])(implicit A: Fractional[A]): A = {

    values.size match {
      case 0 => A.fromInt(0)
      case _ => varianceWithDivisor(values.size)(values)
    }
  }

  def sampleVariance[A](values: Seq[A])(implicit A: Fractional[A]): A = {

    values.size match {
      case 0 | 1 => A.fromInt(0)
      case _ => varianceWithDivisor(values.size - 1)(values)
    }
  }

  def standardDeviation[A](variance: Seq[A] => A)(values: Seq[A])(implicit A: Fractional[A]): Double = {
    sqrt(variance(values).toDouble)
  }

  def populationStandardDeviation[A](values: Seq[A])(implicit A: Fractional[A]): Double = {
    standardDeviation(populationVariance[A])(values)
  }

  def sampleStandardDeviation[A](values: Seq[A])(implicit A: Fractional[A]): Double = {
    standardDeviation(sampleVariance[A])(values)
  }
}
