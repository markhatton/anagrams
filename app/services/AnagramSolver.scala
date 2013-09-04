/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

class AnagramSolver(_dictionary: Set[String]) {

  lazy val dictionary = _dictionary.map(_.toLowerCase())

  def solve(s: String): List[String] =
    solve("", Nil, s.toList.filter(_ != ' ').map(_.toLower))

  def solve(x: String, acc: List[String], xs: List[Char]): List[String] =
    xs match {
      case Nil =>
        if (dictionary.contains(x))
          (x :: acc).reverse.mkString(" ") :: Nil
        else
          Nil
      case _ =>
        val s = x + xs.head
        if (dictionary.contains(s))
          solve("", s :: acc, xs.tail) ::: solve(s, acc, xs.tail)
        else
          solve(s, acc, xs.tail)
    }

}
