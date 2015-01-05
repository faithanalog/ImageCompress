package me.ukl

import javax.imageio.ImageIO
import java.io.File
import java.nio.ByteBuffer
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.nio.ByteOrder
import java.io.FileOutputStream
import java.io.BufferedOutputStream

object Encoder {
  
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("Args format: <action> <action_args>")
      return
    }
    
    val act = args(0).toLowerCase
    act match {
      case "encode" => encodeCmd(args.slice(1, args.length))
      case "decode" => Decoder.decodeCmd(args.slice(1, args.length))
      case _ => println(s"Invalid action: $act")
    }
  }
  
  def encodeCmd(args: Array[String]) {
    if (args.length != 3) {
      println("Args format: encode <infile> <outfile> <target_size>")
      return
    }
    val inFile  = args(0)
    val outFile = args(1)
    val targetSize = Integer.parseInt(args(2))
    
    val img = ImageIO.read(new File(inFile))
    val(blocks, blocksX, _) = ImgBlock.readBlocks(img)
    val(huffBytes, lum, chrom) = encodeBlocks(blocks, targetSize - 64 - 64 - 1)
    
    val out = new BufferedOutputStream(new FileOutputStream(outFile))
    for (q <- List(lum, chrom); x <- q) out.write(x)
    out.write(blocksX)
    out.write(huffBytes)
    out.close()
  }
  
  def encodeBlocks(blocks: Array[ImgBlock], lumQ: Array[Int], chromQ: Array[Int]): Array[Byte] = {
    for (block <- blocks.par) { 
      block.toYCbCr()
      block.toDCT(lumQ, chromQ)
    }
    
    val rleBytes = for (block <- blocks.par) yield RLE.rleBlock(block)
    val rleSize = rleBytes.foldLeft(0) { (l, r) => l + r.length }
    
    val outStream = new ByteArrayOutputStream
    Huffman.huffmanEncode(rleBytes.seq.flatten, outStream)
    
    val huffBytes = outStream.toByteArray()
    huffBytes
  }
  
  def encodeBlocks(blocks: Array[ImgBlock], targetSize: Int): (Array[Byte], Array[Int], Array[Int]) = {
    
    def encodeQ(q: Double) = {
      val base = (blocks.map { b => b.copy }).toArray
      val lMat = QTables.getLum(q)
      val cMat = QTables.getChrom(q)
      encodeBlocks(base, lMat, cMat)
    }
    
    //Initial guess
    val guesses = for (i <- 0 to 5) yield {
      Math.abs(encodeQ(i * 0.2).length - targetSize)
    }
    
    var interval = 0.5
    var bytes: Array[Byte] = null
    var q = guesses.seq.zipWithIndex.sortWith { (l, r) =>
      l._1 < r._1
    }.head._2 * 0.2
    
    def modQ(amnt: Double) {
      q = Math.min(1.0, Math.max(0.00, q + amnt))
      bytes = encodeQ(q)
    }
    
    modQ(0.0)
    
    for (i <- 1 to 20) {
      if (bytes.length > targetSize) {
        modQ(-interval)
      } else {
        modQ(interval)
      }
      interval = interval * 0.7
    }
    
    //If we're below, increase until we're above
    while (bytes.length < targetSize) {
      modQ(interval)
    }
    
    //Reduce until we're back below. This way we never overshoot
    while (bytes.length > targetSize) {
      if (q == 0.0) {
        throw new RuntimeException("Could not reach target size")
      }
      modQ(-interval)
    }
    
    (bytes, QTables.getLum(q), QTables.getChrom(q))
  }

}

//Used for profiling
object Timer {
  private var startTime = 0L
  
  def start() {
    startTime = System.nanoTime()
  }
  
  def end(section: String) {
    val seconds = (System.nanoTime() - startTime) / 1000000000.0
    println(s"$section: $seconds")
  }
}