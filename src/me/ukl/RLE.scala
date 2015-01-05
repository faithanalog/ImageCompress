package me.ukl

import java.nio.ByteBuffer
import java.nio.IntBuffer
import java.nio.ShortBuffer
import java.nio.ByteOrder

object RLE {
  
  //Hard-coded indices to zig-zag across a 8x8 block
  val zigzagIndices = List(
      0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
      0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
      0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
      0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
      0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
      0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
      0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
      0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f
  )
  
  //RLE encodes runs of zeroes
  def rleChannel(chan: Array[Int]) = {    
    //Worst case scenario
    val out = ByteBuffer.allocate(chan.length * 3).order(ByteOrder.BIG_ENDIAN)
    
    //Encode runs of zeroes
    var runC = 0
    for (i <- zigzagIndices) {
      val x = chan(i)
      if (x == 0) {
        runC += 1
      } else {
        if (runC > 0) {
          out.putShort(0)
          out.put(runC.toByte)
          runC = 0
        }
        out.putShort(x.toShort)
      }
    }
    if (runC > 0) {
      out.putShort(0)
      out.put(runC.toByte)
    }
    out.array.slice(0, out.position)
  }
  
  def unrleChannel(in: ByteBuffer, chan: Array[Int]) {
    var value = 0
    var count = 0
    for (i <- zigzagIndices) {
      if (count == 0) {
        value = in.getShort
        count = (if (value == 0) in.get & 0xFF else 1)
      }
      chan(i) = value
      count -= 1
    }
  }
  
  //Same as rleChannel, but channels are interleaved
  def rleBlock(blk: ImgBlock) = {    
    //Worst case scenario
    val out = ByteBuffer.allocate(blk.rChan.length * 3 * 3).order(ByteOrder.BIG_ENDIAN)
    
    //Encode runs of zeroes
    var runC = 0
    for (i <- zigzagIndices; chan <- blk.channels) {
      val x = chan(i)
      if (x == 0) {
        runC += 1
      } else {
        if (runC > 0) {
          out.putShort(0)
          out.put(runC.toByte)
          runC = 0
        }
        out.putShort(x.toShort)
      }
    }
    if (runC > 0) {
      out.putShort(0)
      out.put(runC.toByte)
    }
    out.array.slice(0, out.position)
  }
  
  def unrleBlock(in: ByteBuffer, blk: ImgBlock) {
    var value = 0
    var count = 0
    for (i <- zigzagIndices; chan <- blk.channels) {
      if (count == 0) {
        value = in.getShort
        count = (if (value == 0) in.get & 0xFF else 1)
      }
      chan(i) = value
      count -= 1
    }
  }

}