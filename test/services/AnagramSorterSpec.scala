/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito

class AnagramSorterSpec extends SpecificationWithJUnit with Mockito {

  "An Anagram Sorter" should {

    val unigrams = mock[BinarySearchCSV]
    val presenter = mock[AnagramPresenter]
    val sorter = new AnagramSorter(unigrams, presenter)

    unigrams.find("a") returns Some(1L)
    unigrams.find("b") returns Some(2L)
    unigrams.find("c") returns Some(3L)
    unigrams.find("ab") returns Some(10L)
    unigrams.find("bc") returns Some(100L)

    presenter.present(anyString) answers { s => (s.asInstanceOf[String], 1L) }

    "sort by the shortest number of words then minimum csv unigram value, descending and stable" in {
      sorter.sort(List("a b c", "a bc", "ab c")) must_== List("ab c", "a bc", "a b c")
    }

  }

}
