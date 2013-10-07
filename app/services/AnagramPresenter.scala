/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import collection.mutable

class AnagramPresenter(unigrams: CSV, bigrams: CSV, trigrams: CSV) {

  private final def getNgram(n: Int, bigram: String) = {
    val ngrams = n match {
      case 1 => unigrams
      case 2 => bigrams
      case 3 => trigrams
    }
    ngrams.find(bigram).getOrElse(0L)
  }

  private final def getNgramWithMemo(n: Int, s: String, memo: mutable.Map[String, Long]) = memo.getOrElse(s, {
    val freq = getNgram(n, s)
    memo.put(s, freq)
    freq
  })

  def present(s: String, memo: mutable.Map[String, Long]): (String, Long) = {

    val ss = s.split(' ')

    def fallbackNgramScore(n: Int) = {
      val max = ss.permutations.take(10000).toList.map{ p =>
        val score = p.sliding(n).map(s1 => getNgramWithMemo(n, s1.mkString(" "), memo)).min
        (p, score)
      }.maxBy{ case (_, score) => score }
  
      max._1.mkString(" ") -> max._2 / 
        (if (ss.length <= 2) 1L
         else math.pow(ss.length, 6).toLong)
    }
    
    def ngramsScore(n: Int) = {
      val xs = ss.permutations.toList.map{ p =>
        val s1 = p.mkString(" ")
        s1 -> getNgram(n, s1)
      }
      if (n == 3) println(xs)
      xs.maxBy{ case (_, score) => score }
    }

    ss.length match {
      case n@(1 | 2 | 3) => 
        val scored = ngramsScore(n)
        if (scored._2 > 0) scored else fallbackNgramScore(2)
      case _ => 
        val scored = fallbackNgramScore(3)
        if (scored._2 > 0) scored else fallbackNgramScore(2)
        scored
    }
  }

}
