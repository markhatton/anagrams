/*
 * Copyright (c) 2013 zeebox
 */
package services

class AnagramPresenter(unigrams: BinarySearchCSV, bigrams: BinarySearchCSV) {

  def present(s: String): (String, Long) = {
    val ss = s.split(' ')

    ss.length match {
      case 1 =>
        (s, unigrams.find(s).getOrElse(1L))
      case 2 =>
        (s, bigrams.find(s).getOrElse(1L))
      case _ =>
        val max = ss.permutations.toList.map{ p =>
          val score = p.sliding(2).map(pair => bigrams.find(pair.mkString(" ")).getOrElse(1L)).min
          (p, score)
        }.sortBy{ case (_, score) => score }.last

      (max._1.mkString(" "), max._2 / math.pow(ss.length, 4).toLong)
    }
  }

}
