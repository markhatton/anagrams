/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import collection.mutable

class AnagramSorter(unigrams: BinarySearchCSV, presenter: AnagramPresenter) {

  private def min(s: Array[String], memo: mutable.Map[String, Long]) =
    s.map{ w =>
      memo.getOrElse (w, {
          val freq = unigrams.find(w).getOrElse(1L)
          memo.put(w, freq)
          freq
        })
    }.min

  private def partitionAndRemove(xs: mutable.ListBuffer[String], n: Int): Option[String] = {
    val x = xs.filter(_.split(' ').length == n).headOption
    x foreach(xs -= _)
    x
  }

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

    val results = mutable.ListBuffer[String]()
    val input = mutable.ListBuffer[String](sorted:_*)
    var n = 1
    while (input.nonEmpty && results.length < 110) {
      val x = partitionAndRemove(input, n)
      x foreach results.+=
      n = (n + 1) % 10
    }

    results.toList.map(presenter.present).sortBy{ case (_, score) => -score }.map(_._1)
  }
}
