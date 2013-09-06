/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import org.ardverk.collection.{StringKeyAnalyzer, PatriciaTrie}
import scala.collection.mutable
import scala.annotation.tailrec

class AnagramSolver(_dictionary: Set[String]) {

  val trie = new PatriciaTrie[String, Unit](StringKeyAnalyzer.INSTANCE)
  _dictionary.map(_.toLowerCase()) foreach (s => trie.put(s, ()))

  def dictionary(s: String) = trie.select(s).getKey == s
  def prefix(s: String) = trie.select(s).getKey.startsWith(s)

  def solve(s: String, limit: Int = 1000, timeoutMillis: Int = 5000): List[String] = {
    val chars = s.toLowerCase().toList.filter { c =>
      c >= 'a' && c <= 'z'
    }

    val frontier = mutable.PriorityQueue[(String, List[String], List[Char])]()(Ordering.by{case (_, acc, _) => -acc.length})
    frontier += (("", Nil, chars))

    solve(mutable.ListBuffer[String](), frontier, limit, System.currentTimeMillis() + timeoutMillis)
  }

  @tailrec
  private final def solve(acc: mutable.ListBuffer[String], frontier: mutable.PriorityQueue[(String, List[String], List[Char])], limit: Int, timeoutAtMillis: Long): List[String] =
    if (frontier.isEmpty) {
      acc.toList
    } else {
      val (w, ws, avail) = frontier.dequeue()
      avail match {
        case Nil =>
          val solution = (w :: ws).reverse.mkString(" ")
          if (dictionary(w) && !acc.contains(solution)) {
            acc += solution
            if (acc.length > limit) return acc.toList
          }
        case _ =>
          for (i <- 0 to avail.length - 1) yield {

            val c = avail.lift(i).get
            val s = w + c
            lazy val remain = avail.take(i) ::: avail.takeRight(avail.length - i - 1)

            if (dictionary(s))
              frontier ++= Seq(("", s :: ws, remain), (s, ws, remain))
            else if (prefix(s))
              frontier += ((s, ws, remain))
          }

          if (frontier.size % 50 == 0) // only check clock every ~50 iterations
            if (System.currentTimeMillis() > timeoutAtMillis) return acc.toList

      }
      solve(acc, frontier, limit, timeoutAtMillis)
    }

}
