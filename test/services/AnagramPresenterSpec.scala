/*
 * Copyright (c) 2013 zeebox
 */
package services

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito

class AnagramPresenterSpec extends SpecificationWithJUnit with Mockito {

  "An Anagram Presenter" should {

    val unigrams = mock[BinarySearchCSV]
    val bigrams = mock[BinarySearchCSV]

    unigrams.find("a") returns Some(99)

    val input = "a b c"
    bigrams.find("a b") returns None
    bigrams.find("a c") returns Some(81)
    bigrams.find("b a") returns Some(162)
    bigrams.find("b c") returns Some(243)
    bigrams.find("c a") returns Some(324)
    bigrams.find("c b") returns Some(405)

    val presenter = new AnagramPresenter(unigrams, bigrams)

    "sort words in input using maximum scoring bigrams permutation" in {
      val result = presenter.present(input)
      result must_== ("b c a", 243 / (3 * 3 * 3 * 3))
    }

    "lookup unigrams for single-word anagrams" in {
      val result = presenter.present("a")
      result must_== ("a", 99)
    }

  }

}
