package me.ukl

object DCT {
  
  //Table of values for c0/c1 for used in sumDCT and sumInverseDCT.
  private val cTable = (for (x <- 0 to 7; u <- 0 to 7) yield {
    Math.cos(((2 * x + 1) * u * Math.PI) / 16.0)
  }).toArray
  
  def applyFullDCT(xs: Array[Int], qMat: Array[Int]) = {
    val shifted = for (x <- xs) yield x - 128
    val dct = applyDCT(shifted)
    quantize(dct, qMat, xs)
  }
  
  /**
   * Computes the discrete cosine transform of the input data
   * See formula here http://en.wikipedia.org/wiki/JPEG#Discrete_cosine_transform
   */
  private def applyDCT(xs: Array[Int]) = {
    for (v <- 0 to 7; u <- 0 to 7) yield {
      val aU = alpha(u)
      val aV = alpha(v)
      0.25 * aU * aV * sumDCT(xs, u, v)
    }
  }
  
  //Multiplied by the value at (u,v) for summing
  //Converted to array because that makes it much faster to access
  private val sumTable = (for (v <- 0 to 7; u <- 0 to 7; y <- 0 to 7; x <- 0 to 7) yield {
    cTable(x * 8 + u) * cTable(y * 8 + v)
  }).toArray
  
  private def sumDCT(xs: Array[Int], u: Int, v: Int) = {
    var sum = 0.0;
    val sumTableOffs = v * (8 * 8 * 8) + u * (8 * 8)
    for (y <- 0 to 7; x <- 0 to 7) {
      val i = x + y * 8
      sum += xs(i) * sumTable(sumTableOffs + i)
    }
    sum
  }
  
  private def alpha(u: Int) = {
    if (u == 0)
      0.7071067811865475 // 1 / sqrt(2)
    else
      1.0
  }
  
  private def quantize(dct: Seq[Double], qMat: Array[Int], out: Array[Int]) = {
    var i = 0
    for (in <- dct) {
      out(i) = Math.round(in / qMat(i).toDouble).toInt
      i += 1
    }
  }
  
  def applyFullInverseDCT(xs: Array[Int], qMat: Array[Int]) = {
    val in = (for (i <- 0 until xs.length) yield xs(i) * qMat(i)).toArray
    val invDCT = applyInverseDCT(in)
    var i = 0
    for (in <- invDCT) {
      xs(i) = Math.max(Math.min(127, in), -128) + 128
      i += 1
    }
  }
  
  private def applyInverseDCT(in: Array[Int]) = {
    for (y <- 0 to 7; x <- 0 to 7) yield sumInverseDCT(in, x, y).toInt
  }
  
  
  //Multiplied by the value at (x,y) for summing
  //Converted to array because that makes it much faster to access
  private val inverseSumTable = (for (y <- 0 to 7; x <- 0 to 7; v <- 0 to 7; u <- 0 to 7) yield {
      val aU = alpha(u)
      val aV = alpha(v)
      val c0 = cTable(x * 8 + u)
      val c1 = cTable(y * 8 + v)
      aU * aV * c0 * c1
  }).toArray
  
  private def sumInverseDCT(in: Array[Int], x: Int, y: Int) = {
    val sumTableOffs = y * (8 * 8 * 8) + x * (8 * 8)
    var sum = 0.0
    for (v <- 0 to 7; u <- 0 to 7) {
      val i = u + v * 8
      sum += in(i) * inverseSumTable(sumTableOffs + i)
    }
    sum * 0.25
  }
}
