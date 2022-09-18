package com.ScalaAndStats


object Averages {

  def mean[A](values: Seq[A])(implicit A: Fractional[A]): A = {
    if (values.isEmpty) A.fromInt(0) else A.div(values.sum, A.fromInt(values.size))
  }
}
