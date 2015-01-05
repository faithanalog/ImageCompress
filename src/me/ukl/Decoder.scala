package me.ukl

import java.io.InputStream
import java.nio.ByteBuffer
import javax.imageio.ImageIO
import java.nio.ByteOrder
import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.ArrayBuffer

object Decoder {
  
  def decodeCmd(args: Array[String]) {
    if (args.length != 2) {
      println("Args format: decode <infile> <outfile.png>")
      return
    }
    val inFile  = args(0)
    val outFile = args(1)
    
    val in = new BufferedInputStream(new FileInputStream(inFile))
    val(blocks, blocksX) = decodeBlocks(in)
    
    val img = ImgBlock.imgFromBlocks(blocks, blocksX)
    ImageIO.write(img, "png", new File(outFile))
  }
  
  def decodeBlocks(in: InputStream) = { 
    val lum = new Array[Int](64)
    val chrom = new Array[Int](64)
    
    //Read luminance and chrominance Q tables
    for (i <- 0 until 64) {
      lum(i) = in.read & 0xFF
    }
    for (i <- 0 until 64) {
      chrom(i) = in.read & 0xFF
    }
    
    //Amount of blocks on the X axis
    val blocksX = in.read & 0xFF
    
    val huffman = Huffman.huffmanDecode(in).toArray
    val inBytes = ByteBuffer.wrap(huffman).order(ByteOrder.BIG_ENDIAN)
    
    val blockBuilder = new ArrayBuffer[ImgBlock]
    while (inBytes.hasRemaining) {
      val block = new ImgBlock()
      RLE.unrleBlock(inBytes, block)
      blockBuilder += block
    }
    val blocks = blockBuilder.toArray
    
    for (block <- blocks.par) {
      block.fromDCT(lum, chrom)
      block.fromYCbCr() 
    }
    (blocks, blocksX)
  }

}