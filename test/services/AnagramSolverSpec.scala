/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito

class AnagramSolverSpec extends SpecificationWithJUnit with Mockito {

  val dictionary = Set("a", "b", "c", "ab", "bc")

  val unigrams = mock[BinarySearchCSV]
  unigrams.find(any) returns None

  val anagramSolver = new NativeAnagramSolver(dictionary, unigrams)

  val solutions = Set("a b c", "a bc", "ab c")

  "An Anagrams Solver" should {

    "solve unique anagrams"  in {
      anagramSolver.solve("a") must_== List("a")
      anagramSolver.solve("abc").toSet must_== solutions
      anagramSolver.solve("bac").toSet must_== solutions
    }

    "deduplicate anagrams when repeated letters in input" in {
      anagramSolver.solve("aa") must_== List("a a")
    }

    "disregard non-alpha characters in input" in {
      anagramSolver.solve("a'b c! 100.").toSet must_== solutions
    }

    "ignore case" in {
      new NativeAnagramSolver(dictionary map (_.toUpperCase()), unigrams).solve("AbC").toSet must_== solutions
    }

    "return no solutions" in {
      anagramSolver.solve("abcd") must_== Nil
    }

  }

}
