package me.ukl

import java.awt.image.BufferedImage
import java.awt.Transparency
import java.awt.image.DataBufferInt

class ImgBlock {

  //When this is YCbCr,
  //Y stored in rChan, Cb stored in gChan, Cr stored in bChan
  val rChan = new Array[Int](8 * 8)
  val gChan = new Array[Int](8 * 8)
  val bChan = new Array[Int](8 * 8)

  val channels = List(rChan, gChan, bChan)
  
  def copy = {
    val newBlock = new ImgBlock
    System.arraycopy(this.rChan, 0, newBlock.rChan, 0, 64)
    System.arraycopy(this.gChan, 0, newBlock.gChan, 0, 64)
    System.arraycopy(this.bChan, 0, newBlock.bChan, 0, 64)
    newBlock
  }
  
  //http://en.wikipedia.org/wiki/YCbCr#JPEG_conversion
  def toYCbCr() = {
    for (i <- 0 to 63) {
      
      //Quantize to 16 bit color
      val r = rChan(i)
      val g = gChan(i)
      val b = bChan(i)
      
      rChan(i) = Math.round(      (0.299    * r) + (0.587    * g) + (0.114    * b)).toInt
      gChan(i) = Math.round(128 - (0.168736 * r) - (0.331264 * g) + (0.5      * b)).toInt
      bChan(i) = Math.round(128 + (0.5      * r) - (0.418688 * g) - (0.081312 * b)).toInt
      
//      println(s"(${rChan(i)},${gChan(i)},${bChan(i)})")
    }
    
    //Clamp values
    for (chan <- List(rChan, gChan, bChan)) {
      chan.transform { x => Math.max(Math.min(x, 255), 0) }
    }
  }
  
  def fromYCbCr() = {
    for (i <- 0 to 63) {
      val y  = rChan(i)
      val cb = gChan(i) - 128
      val cr = bChan(i) - 128
      
      rChan(i) = Math.round(y                + 1.402   * cr).toInt
      gChan(i) = Math.round(y - 0.34414 * cb - 0.71414 * cr).toInt
      bChan(i) = Math.round(y + 1.772   * cb               ).toInt
    }
    
    //Clamp values
    for (chan <- List(rChan, gChan, bChan)) {
      chan.transform { x => Math.max(Math.min(x, 255), 0) }
    }
  }
  
  def toDCT(lum: Array[Int], chrom: Array[Int]) = {
    DCT.applyFullDCT(rChan, lum)
    DCT.applyFullDCT(gChan, chrom)
    DCT.applyFullDCT(bChan, chrom)
  }
  
  def fromDCT(lum: Array[Int], chrom: Array[Int]) = {
    DCT.applyFullInverseDCT(rChan, lum)
    DCT.applyFullInverseDCT(gChan, chrom)
    DCT.applyFullInverseDCT(bChan, chrom)
  }

}

object ImgBlock {
  
  def imgFromBlocks(blocks: Array[ImgBlock], blocksX: Int) = {
    val blocksY = blocks.length / blocksX
    val img = new BufferedImage(blocksX * 8, blocksY * 8, BufferedImage.TYPE_INT_RGB)
    val pixarray = img.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
    
    for (i <- (0 until blocks.length).par) {
      val px = (i % blocksX) * 8
      val py = (i / blocksX) * 8
      val blk = blocks(i)
      
      for (x <- 0 to 7; y <- 0 to 7) {
        val i = x + y * 8
        val r = blk.rChan(i)
        val g = blk.gChan(i)
        val b = blk.bChan(i)
        pixarray((x + px) + (y + py) * img.getWidth) = r << 16 | g << 8 | b
      }
    }
    img
  }
  
  def readBlocks(img: BufferedImage) = {
    val expanded = expandImg(img)
    val pixarray = expanded.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
    
    val blocksX = expanded.getWidth / 8
    val blocksY = expanded.getHeight / 8
    val blocks = new Array[ImgBlock](blocksX * blocksY)
    for (i <- (0 until blocks.length).par) {
      val px = (i % blocksX) * 8
      val py = (i / blocksX) * 8
      val block = new ImgBlock()
      for (x <- 0 to 7; y <- 0 to 7) {
        val rgb = pixarray((x + px) + (y + py) * expanded.getWidth)
        val i = x + y * 8
        block.rChan(i) = (rgb >> 16) & 0xFF
        block.gChan(i) = (rgb >> 8) & 0xFF
        block.bChan(i) = rgb & 0xFF
      }
      blocks(i) = block
    }
    (blocks, blocksX, blocksY)
  }
  
  private def expandImg(img: BufferedImage): BufferedImage = {
    //Remove alpha if any
    var opaque: BufferedImage = null;
    if (img.getTransparency != Transparency.OPAQUE) {
      opaque = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_INT_RGB)
      val gfx = opaque.createGraphics()
      gfx.drawImage(img, 0, 0, null)
      gfx.dispose()
      opaque = img
    } else {
      opaque = img
    }
    
    var width = img.getWidth
    var height = img.getHeight
    
    //Ensure the new widths are multiples of 8
    if ((width & 7) != 0)
      width = (width | 7) + 1

    if ((height & 7) != 0)
      height = (height | 7) + 1
    
    val newImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val gfx = newImg.createGraphics()
    
    //If we increased image size, fill the extra space with the edge pixel colors
    for (x <- width - img.getWidth to 0 by -1;
         y <- height - img.getHeight to 0 by -1) {
      gfx.drawImage(img, x, y, null)
    }
    gfx.dispose()
    newImg
  }
  
}
