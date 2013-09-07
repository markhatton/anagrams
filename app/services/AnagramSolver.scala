/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import org.ardverk.collection.{StringKeyAnalyzer, PatriciaTrie}
import scala.collection.mutable
import scala.annotation.tailrec

class AnagramSolver(_dictionary: Set[String], unigrams: BinarySearchCSV) {

  private final val trie = {
    val t = new PatriciaTrie[String, Long](StringKeyAnalyzer.INSTANCE)
    _dictionary.map(_.toLowerCase()) foreach (s => t.put(s, unigrams.find(s).getOrElse(1L)))
    t
  }

  @inline
  private final def dictionary(s: String): Long =
    if (trie.containsKey(s)) trie.selectValue(s) else 0

  @inline
  private final def prefix(s: String) = trie.selectKey(s).startsWith(s)

  private final val k = 100000000L

  def solve(s: String, limit: Int = 5000, timeoutMillis: Int = 5000): List[String] = {
    val chars = s.toLowerCase().toList.filter { c => c >= 'a' && c <= 'z'}

    val frontier = mutable.PriorityQueue[(String, List[String], List[Char], Long)]()(Ordering.by{case (w, ws, av, priority) => priority})
    frontier += (("", Nil, chars, 0))

    solve(mutable.ListBuffer[String](), frontier, mutable.HashSet[String](), limit, System.currentTimeMillis() + timeoutMillis)
  }

  @tailrec
  private final def solve(acc: mutable.ListBuffer[String], frontier: mutable.PriorityQueue[(String, List[String], List[Char], Long)], solved: mutable.HashSet[String], limit: Int, timeoutAtMillis: Long): List[String] =
    if (frontier.isEmpty)
      acc.toList
    else {
      val (w, ws, avail, priority) = frontier.dequeue()

      if (avail.isEmpty) {
        val solution = (w :: ws).sorted.mkString(" ")
        if (dictionary(w) > 0) {
          acc += solution
          if (acc.length >= limit) return acc.toList
        }
      } else {
        for (c <- avail.distinct) {
          val s = w + c
          lazy val remain = avail diff List(c)

          val solution = (s :: ws).sorted.mkString(" ")
          if (!solved.contains(solution)) {
            solved.add(solution)

            val unigramFreq = dictionary(s)
            if (unigramFreq > 0) {
              val freqs = unigramFreq :: ws.map(dictionary)
              val _priority = -(ws.length + 1) * k + freqs.min
              frontier += (("", s :: ws, remain, _priority))
            }
            if (prefix(s))
              frontier += ((s, ws, remain, priority))
          }
        }

      }

      if (frontier.length % 100 == 0 // only check clock every ~100 iterations
        && System.currentTimeMillis() > timeoutAtMillis) return acc.toList

      solve(acc, frontier, solved, limit, timeoutAtMillis)
    }

}
