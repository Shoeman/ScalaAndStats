package com.ScalaAndStats

import com.ScalaAndStats.Variance.{populationStandardDeviation, populationVariance}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VarianceTest extends AnyFlatSpec with Matchers {

  val TOLERANCE = 0.001

  behavior of "VarianceTest"

  "populationVariance" should "handle empty sequence" in {
    val values: Seq[Double] = Seq()
    populationVariance(values) should equal(0)
  }

  "populationVariance" should "handle single item" in {
    val values: Seq[Double] = Seq(5)
    populationVariance(values) should equal(0)
  }

  "populationVariance" should "handle zero variance" in {
    val values: Seq[Double] = Seq(2, 2, 2, 2)
    populationVariance(values) should equal(0)
  }

  "populationVariance" should "handle simple variance" in {
    val values: Seq[Double] = Seq(1, 1, 1, 5, 5, 5)
    populationVariance(values) should equal(4)
  }

  "populationVariance" should "handle floating point variance" in {
    val values: Seq[Double] = Seq(1, 2, 3, 4, 5, 6)
    populationVariance(values) should equal(2.917 +- TOLERANCE)
  }

  "populationStandardDeviation" should "handle empty sequence" in {
    val values: Seq[Double] = Seq()
    populationStandardDeviation(values) should equal(0)
  }

  "populationStandardDeviation" should "handle single item" in {
    val values: Seq[Double] = Seq(1)
    populationStandardDeviation(values) should equal(0)
  }

  "populationStandardDeviation" should "handle zero variance" in {
    val values: Seq[Double] = Seq(2, 2, 2, 2)
    populationStandardDeviation(values) should equal(0)
  }

  "populationStandardDeviation" should "handle simple variance" in {
    val values: Seq[Double] = Seq(1, 1, 1, 5, 5, 5)
    populationStandardDeviation(values) should equal(2)
  }

  "populationStandardDeviation" should "handle floating point variance" in {
    val values: Seq[Double] = Seq(1, 2, 3, 4, 5, 6)
    populationStandardDeviation(values) should equal(1.707 +- TOLERANCE)
  }
}
