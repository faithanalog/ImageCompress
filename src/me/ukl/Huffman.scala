package me.ukl

import scala.collection.mutable.PriorityQueue
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.io.ByteArrayOutputStream
import java.io.InputStream
import scala.annotation.tailrec
import java.io.OutputStream

object Huffman {
  
  def huffmanEncode(d: Seq[Byte], out: OutputStream) {
    
    //Count the frequency of each value
    val freqs = new Array[Int](256)
    for (x <- d) {
      freqs(x & 0xFF) += 1
    }
    
    val initialNodes = freqs.zipWithIndex
      .filter { case (freq, _) => freq != 0 }
      .map { case (freq, value) => Leaf(freq, value) }
    
    //Build the huffman compression tree
    implicit val LeafOrdering = new Ordering[Node] {
      //Highest priority (lowest number) first
      override def compare(a: Node, b: Node) = b.freq compare a.freq
    }
    var freqQueue = new PriorityQueue[Node]
    freqQueue ++= initialNodes
    
    while (freqQueue.size > 1) {
      val left = freqQueue.dequeue()
      val right = freqQueue.dequeue()
      val branch = new Branch(left, right)
      
      
      left.parent = branch
      right.parent = branch
      left.isRight = false
      right.isRight = true
      freqQueue += branch
    }
    
    val root = freqQueue.dequeue()
    
    //Find huffman code for each value
    val huffCodes = new Array[HuffmanCode](256)
    
    for (leaf <- initialNodes) {
      var curNode: Node = leaf
      var code = 0
      var codeLen = 0
      while (curNode != root) {
        code >>>= 1
        //1 means branch right, 0 means branch left
        code |= (if (curNode.isRight) 0x80000000 else 0)
        codeLen += 1
        curNode = curNode.parent
      }
      
      //Shift bits so they're right aligned
      code >>>= 32 - codeLen
      huffCodes(leaf.value) = new HuffmanCode(code, codeLen)
    }
    
    
    val nodeBuff = ByteBuffer.allocate(4096).order(ByteOrder.LITTLE_ENDIAN)
    root.write(nodeBuff)
    nodeBuff.flip()
    
    //Write size of tree, followed by tree. Size is used to skip to data
    out.write(nodeBuff.limit & 0xFF)
    out.write((nodeBuff.limit >> 8) & 0xFF)
    out.write(nodeBuff.array, 0, nodeBuff.limit)
    
    //Write size of data when decompressed
    out.write(d.length & 0xFF)
    out.write((d.length >> 8) & 0xFF)
    out.write((d.length >> 16) & 0xFF)
    
    //Write out huffman-coded data
    val bits = new BitOutputStream(out)
    for (x <- d) {
      val code = huffCodes(x & 0xFF)
      bits.writeBits(code.code, code.codeLen)
    }
    bits.flush()
  }
  
  def huffmanDecode(in: InputStream) = {
    val treeSize = (in.read | (in.read << 8)) & 0xFFFF
    val treeBytes = new Array[Byte](treeSize)
    in.read(treeBytes)
    val root = Node.readNode(ByteBuffer.wrap(treeBytes).order(ByteOrder.LITTLE_ENDIAN))
    
    val outSize = (in.read | (in.read << 8) | (in.read << 16)) & 0xFFFFFF
    
    val bits = new BitInputStream(in)
    for (_ <- 0 until outSize) yield root.readValue(bits)
  }

}

class HuffmanCode(val code: Int, val codeLen: Int)

abstract class Node {
  var parent: Node = null
  var isRight = false
  val freq: Int
  
  /**
   * Writes the node's byte representation to the ByteBuffer
   */
  def write(out: ByteBuffer)
  
  /**
   * Reads a huffman code from the stream, traversing the tree to find value
   */
  def readValue(bits: BitInputStream): Byte
}

case class Branch(val left: Node, val right: Node) extends Node {
  val freq = left.freq + right.freq
  
  def write(out: ByteBuffer) {
    out.put(0.toByte)                  //Signal that this is not a leaf
    val pos = out.position             //Save position before right node
    out.position(pos + 2)              //Reserve 2 bytes for the offset to the right node
    left.write(out)
    val lsize = out.position - pos - 2 //Amount of bytes written by left node
    out.putShort(pos, lsize.toShort)
    right.write(out)
  }
  
  def readValue(bits: BitInputStream) = {
    if (bits.readBit() == 1)
      right.readValue(bits)
    else
      left.readValue(bits)
  }
}
case class Leaf(val freq: Int, val value: Int) extends Node {
  def write(out: ByteBuffer) {
    out.put(1.toByte)     //Signal that this is a leaf
    out.put(value.toByte)
  }
  
  def readValue(bits: BitInputStream) = value.toByte
}

object Node {
  def readNode(in: ByteBuffer): Node = {
    val nodeType = in.get & 0xFF
    if (nodeType == 0) {
      in.getShort //Offset to right node, unused in this code (should be useful for z80 ASM decoder)
      Branch(readNode(in), readNode(in))
    } else {
      Leaf(0, in.get & 0xFF)
    }
  }
}