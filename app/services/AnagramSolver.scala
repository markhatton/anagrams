/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import org.ardverk.collection.{StringKeyAnalyzer, PatriciaTrie}
import scala.collection.mutable
import scala.annotation.tailrec
import play.api.Logger

class AnagramSolver(_dictionary: Set[String], unigrams: BinarySearchCSV) {

  private final val trie = {
    val t = new PatriciaTrie[String, Long](StringKeyAnalyzer.INSTANCE)
    _dictionary.map(_.toLowerCase()) foreach (s => t.put(s, unigrams.find(s).getOrElse(1L)))
    t
  }

  private final val k = 100000000L

  var iterations: Long = 0L // TODO: Mark 08/09/2013 - not thread safe

  def solve(s: String, limit: Int = 50000, timeoutMillis: Int = 5000): List[String] = {
    val chars = s.toLowerCase().toList.filter { c => c >= 'a' && c <= 'z'}

    val frontier = mutable.PriorityQueue[(String, List[String], Long, List[Char], Long)]()(Ordering.by{case (w, ws, score, av, priority) => priority})
    frontier += (("", Nil, Long.MaxValue, chars, 0))

    iterations = 0L

    val solutions = solve(mutable.ListBuffer[String](), frontier, mutable.HashSet[String](), limit, System.currentTimeMillis() + timeoutMillis)
    Logger.info(s"Anagram [$s], found ${solutions.length} solutions using approx. $iterations iterations")
    solutions
  }

  private final def solve(acc: mutable.ListBuffer[String], frontier: mutable.PriorityQueue[(String, List[String], Long, List[Char], Long)], solved: mutable.HashSet[String], limit: Int, timeoutAtMillis: Long): List[String] = {

    while (!frontier.isEmpty && acc.length < limit && System.currentTimeMillis() < timeoutAtMillis) {
      val (w, ws, score, avail, priority) = frontier.dequeue()

      iterations = iterations + 1

      for (c <- avail.distinct) {
        val s = w + c
        lazy val remain = avail diff List(c)

        val prefix = trie.select(s)

        if (prefix.getKey.startsWith(s)) {
          frontier += ((s, ws, score, remain, priority))

          if (prefix.getKey == s && prefix.getValue > 0) {
            val solution = (s :: ws).sorted.mkString(" ")
            if (!solved.contains(solution)) {
              solved.add(solution)
              lazy val newScore = math.min(prefix.getValue, score)
              lazy val newPriority = -(ws.length + 1) * k + newScore

              if (remain.isEmpty) {
                acc += solution
              } else {
                frontier += (("", s :: ws, newScore, remain, newPriority))
              }
            }
          }

        }
      }

    }
    acc.toList
  }

}
