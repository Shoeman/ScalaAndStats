package com.ScalaAndStats

import com.ScalaAndStats.Averages.mean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AveragesTest extends AnyFlatSpec with Matchers {

  it should "handle empty sequences" in {
    val values: Seq[Double] = Seq()
    mean(values) should equal(0)
  }

  it should "handle single item" in {
    val values: Seq[Double] = Seq(5)
    mean(values) should equal(5)
  }

  it should "handle multiple integers" in {
    val values: Seq[Double] = Seq(1, 2)
    mean(values) should equal(1.5)
  }

  it should "handle floating point values" in {
    val values: Seq[Double] = Seq(0.25, 0.5, 0.75)
    mean(values) should equal(0.5)
  }
}
