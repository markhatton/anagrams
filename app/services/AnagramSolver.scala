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

  private final val k = 100000000L

  def solve(s: String, limit: Int = 5000, timeoutMillis: Int = 5000): List[String] = {
    val chars = s.toLowerCase().toList.filter { c => c >= 'a' && c <= 'z'}

    val frontier = mutable.PriorityQueue[(String, List[String], Long, List[Char], Long)]()(Ordering.by{case (w, ws, score, av, priority) => priority})
    frontier += (("", Nil, Long.MaxValue, chars, 0))

    solve(mutable.ListBuffer[String](), frontier, mutable.HashSet[String](), limit, System.currentTimeMillis() + timeoutMillis)
  }

  @tailrec
  private final def solve(acc: mutable.ListBuffer[String], frontier: mutable.PriorityQueue[(String, List[String], Long, List[Char], Long)], solved: mutable.HashSet[String], limit: Int, timeoutAtMillis: Long): List[String] = {
    if (frontier.isEmpty || acc.length >= limit || System.currentTimeMillis() > timeoutAtMillis)
      return acc.toList

    val (w, ws, score, avail, priority) = frontier.dequeue()

    for (c <- avail.distinct) {
      val s = w + c
      lazy val remain = avail diff List(c)
      lazy val solution = (s :: ws).sorted.mkString(" ")

      val prefix = trie.select(s)

      if (prefix.getKey == s && prefix.getValue > 0 && !solved.contains(solution)) {
        solved.add(solution)
        val newScore = math.min(prefix.getValue, score)
        val newPriority = -(ws.length + 1) * k + newScore

        if (remain.isEmpty) {
          acc += solution
        } else {
          frontier += (("", s :: ws, newScore, remain, newPriority))
        }
      }

      if (prefix.getKey.startsWith(s))
        frontier += ((s, ws, score, remain, priority))
    }

    solve(acc, frontier, solved, limit, timeoutAtMillis)
  }

}
