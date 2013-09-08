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
    val k = math.pow(3, 32).toLong
    bigrams.find("a b") returns None
    bigrams.find("a c") returns Some(k)
    bigrams.find("b a") returns Some(2*k)
    bigrams.find("b c") returns Some(3*k)
    bigrams.find("c a") returns Some(4*k)
    bigrams.find("c b") returns Some(5*k)

    val presenter = new AnagramPresenter(unigrams, bigrams)

    "sort words in input using maximum scoring bigrams permutation" in {
      val result = presenter.present(input)
      result must_== ("b c a", 3)
    }

    "lookup unigrams for single-word anagrams" in {
      val result = presenter.present("a")
      result must_== ("a", 99)
    }

  }

}
