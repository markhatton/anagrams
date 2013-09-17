/*
 * Copyright (c) 2013 zeebox
 */
package services

import collection.mutable

class AnagramPresenter(unigrams: BinarySearchCSV, bigrams: BinarySearchCSV) {

  def present(s: String, memo: mutable.Map[String, Long]): (String, Long) = {
    val ss = s.split(' ')

    ss.length match {
      case 1 =>
        (s, unigrams.find(s).getOrElse(1L))

      case _ =>
        def getBigram(bigram: String) = memo.getOrElse(bigram, {
          val freq = bigrams.find(bigram).getOrElse(1L)
          memo.put(bigram, freq)
          freq
        })

        val max = ss.permutations.take(10000).toList.map{ p =>
          val score = p.sliding(2).map(pair => getBigram(pair.mkString(" "))).min
          (p, score)
        }.sortBy{ case (_, score) => score }.last

      (max._1.mkString(" "), max._2 / (if (ss.length <= 2) 1L
                                       else math.pow(ss.length, 6).toLong)
      )
    }
  }

}
