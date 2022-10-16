package com.ScalaAndStats

import com.ScalaAndStats.Variance.{populationStandardDeviation, populationVariance, sampleStandardDeviation, sampleVariance}
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

  "sampleVariance" should "handle empty sequence" in {
    val values: Seq[Double] = Seq()
    sampleVariance(values) should equal(0)
  }

  // Not sure what should really happen in this case
  "sampleVariance" should "handle single item" in {
    val values: Seq[Double] = Seq(5)
    sampleVariance(values) should equal(0)
  }

  "sampleVariance" should "handle zero variance" in {
    val values: Seq[Double] = Seq(2, 2, 2, 2)
    sampleVariance(values) should equal(0)
  }

  "sampleVariance" should "handle simple variance" in {
    val values: Seq[Double] = Seq(1, 1, 1, 5, 5, 5)
    sampleVariance(values) should equal(4.8 +- TOLERANCE)
  }

  "sampleVariance" should "handle complex variance" in {
    val values: Seq[Double] = Seq(1, 2, 3, 4, 5, 6)
    sampleVariance(values) should equal(3.5 +- TOLERANCE)
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

  "sampleStandardDeviation" should "handle empty sequence" in {
    val values: Seq[Double] = Seq()
    sampleStandardDeviation(values) should equal(0)
  }

  "sampleStandardDeviation" should "handle single item" in {
    val values: Seq[Double] = Seq(1)
    sampleStandardDeviation(values) should equal(0)
  }

  "sampleStandardDeviation" should "handle zero variance" in {
    val values: Seq[Double] = Seq(2, 2, 2, 2)
    sampleStandardDeviation(values) should equal(0)
  }

  "sampleStandardDeviation" should "handle simple variance" in {
    val values: Seq[Double] = Seq(1, 1, 1, 5, 5, 5)
    sampleStandardDeviation(values) should equal(2.191 +- TOLERANCE)
  }

  "sampleStandardDeviation" should "handle floating point variance" in {
    val values: Seq[Double] = Seq(1, 2, 3, 4, 5, 6)
    sampleStandardDeviation(values) should equal(1.871 +- TOLERANCE)
  }
}
