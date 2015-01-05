package me.ukl

import java.io.OutputStream
import java.io.InputStream


class BitOutputStream(val out: OutputStream) {
  
  private var curByte = 0
  private var shiftCounter = 0
  
  /**
   * Writes a number of bits from val to the stream
   * @param value Value to write
   * @param numBits number of bits to write from value
   */
  def writeBits(value: Int, numBits: Int) {
    for (i <- 0 until numBits) {
      val bit = (value >> (numBits - i - 1) & 1)
      writeBit(bit)
    }
  }
  
  def writeBit(bit: Int) {
    curByte <<= 1
    curByte |= bit
    shiftCounter += 1
    if (shiftCounter == 8) {
      out.write(curByte)
      shiftCounter = 0
      curByte = 0
    }
  }
  
  def flush() {
    while (shiftCounter != 0) {
      writeBit(0)
    }
  }
  
}

class BitInputStream(val in: InputStream) {
  
  private var curByte = 0
  private var shiftCounter = 0
  
  /**
   * Reads a bit from the input stream
   */
  def readBit() = {
    if (shiftCounter == 0) {
      curByte = in.read() & 0xFF
      shiftCounter = 8
    }
    val resultBit = (curByte & 0x80) >> 7
    curByte <<= 1
    shiftCounter -= 1
    resultBit
  }
  
  def readBits(numBits: Int) {
    var result = 0
    for (i <- 1 to numBits) {
      result = (result << 1) | readBit()
    }
    result
  }

}