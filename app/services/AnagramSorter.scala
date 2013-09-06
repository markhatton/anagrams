/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import collection.mutable

class AnagramSorter(unigrams: BinarySearchCSV) {

  private def min(s: Array[String], memo: mutable.Map[String, Long]) =
    s.map{ w =>
      memo.getOrElse (w, {
          val freq = unigrams.find(w).getOrElse(1L)
          memo.put(w, freq)
          freq
        })
    }.min

  private def partitionAndTake(xs: List[String], p: Int, n: Int): List[String] =
    xs.filter(_.split(' ').length == p).take(n)

  def sort(xs: List[String]): List[String] = {
    val memo = mutable.Map[String, Long]()
    val sorted = xs.sortWith{ case (a, b) =>
      val as = a.split(' ')
      val bs = b.split(' ')
      if (as.length == bs.length)
        min(as, memo) > min(bs, memo)
      else
        bs.length > as.length
    }

    {
      for (n <- 1 to 10) yield partitionAndTake(sorted, n, 25)
    }.flatten.toList
  }
}
