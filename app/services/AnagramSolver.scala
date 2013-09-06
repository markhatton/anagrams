/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import org.ardverk.collection.{StringKeyAnalyzer, PatriciaTrie}

class AnagramSolver(_dictionary: Set[String]) {

  val trie = new PatriciaTrie[String, Unit](StringKeyAnalyzer.INSTANCE)
  _dictionary.map(_.toLowerCase()) foreach (s => trie.put(s, ()))

  def dictionary(s: String) = trie.select(s).getKey == s
  def prefix(s: String) = trie.select(s).getKey.startsWith(s)

  def solve(s: String): List[String] = {
    val chars = s.toLowerCase().toList.filter { c =>
      c >= 'a' && c <= 'z'
    }
    solve("", Nil, chars).distinct
  }

  def solve(x: String, acc: List[String], xs: List[Char]): List[String] =
    xs match {
      case Nil =>
        if (dictionary(x))
          (x :: acc).reverse.mkString(" ") :: Nil
        else
          Nil
      case _ =>
      {
        for (i <- 0 to xs.length - 1) yield {

          val c = xs.lift(i).get
          val s = x + c
          lazy val remain = xs.take(i) ::: xs.takeRight(xs.length - i - 1)

          if (dictionary(s))
            solve("", s :: acc, remain) ::: solve(s, acc, remain)
          else if (prefix(s))
            solve(s, acc, remain)
          else
            Nil
        }
      }.toList.flatten
    }

}
