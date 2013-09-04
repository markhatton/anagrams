/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import org.specs2.mutable.SpecificationWithJUnit

class AnagramSolverSpec extends SpecificationWithJUnit {

  val dictionary = Set("a", "b", "c", "ab", "bc")
  val solutions = Set("a b c", "b a c", "b c a", "b a c", "a c b", "c b a", "a bc", "c a b", "ab c", "bc a", "c ab")

  val anagramSolver = new AnagramSolver(dictionary)

  "An Anagrams Solver" should {

    "solve anagrams"  in {
      anagramSolver.solve("abc").toSet must_== solutions
      anagramSolver.solve("bac").toSet must_== solutions
    }

    "disregard spaces in input" in {
      anagramSolver.solve("a b c").toSet must_== solutions
    }

    "ignore case" in {
      new AnagramSolver(dictionary map (_.toUpperCase())).solve("AbC").toSet must_== solutions
    }

    "return no solutions" in {
      anagramSolver.solve("abcd") must_== Nil
    }

  }

}
