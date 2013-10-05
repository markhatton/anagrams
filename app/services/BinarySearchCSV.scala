/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import java.io.{RandomAccessFile, File}
import java.nio.channels.FileChannel
import java.nio.MappedByteBuffer

trait CSV {
  def find(key: String): Option[Long]
}

/**
 * Loads file into memory and assumes it is a sorted, tab-separated "CSV".  The CSV may then
 * be searched by its key (first column) and its value if found (second column) is returned
 */
class BinarySearchCSV(file: File) extends CSV {

  private final val raFile = new RandomAccessFile(file, "r")

  private final val length = raFile.length.toInt

  private final val buf = new ThreadLocal[MappedByteBuffer] {
    override def initialValue = {
      raFile.getChannel.map(FileChannel.MapMode.READ_ONLY, 0, length)
    }
  }

  buffer.load()

  def find(key: String): Option[Long] = {
    find(key, 0, length)
  }

  private final def buffer = buf.get

  private final def find(key: String, lBound: Integer, uBound: Integer): Option[Long] = {
    if (lBound == uBound) return None
    val midpoint = lBound + (uBound - lBound) / 2

    buffer.position(midpoint)
    rewind()

    if (buffer.position() < lBound) return None

    val (k, v) = {
      val kv = readLine().split('\t')
      (kv(0), kv(1).toLong)
    }

    if (key == k) {
      Some(v)
    } else if (key < k) {
      find(key, lBound, midpoint)
    } else {
      find(key, midpoint, uBound)
    }
  }

  private final def rewind() {
    var pos = buffer.position()
    var b = buffer.get(pos)
    while (b != '\n') {
      pos = pos - 1
      if (pos < 0)
        b = '\n'
      else
        b = buffer.get(pos)
    }
    buffer.position(pos + 1)
  }

  private final def readLine(): String = {
    var b = buffer.get
    val bs = new StringBuilder
    while (b != '\n') {
      bs.append(b.toChar)
      b = buffer.get
    }
    bs.toString()
  }

}
