/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

class AnagramSorter(csv: BinarySearchCSV) {

  private def min(s: Array[String]) =
    s.map(csv.find).flatten.min

  def sort(xs: List[String]): List[String] =
    xs.sortWith{ case (a, b) =>
      val as = a.split(' ')
      val bs = b.split(' ')
      if (as.length == bs.length)
        min(as) > min(bs)
      else
        bs.length > as.length
    }
}
