/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito

class AnagramSorterSpec extends SpecificationWithJUnit with Mockito {

  "An Anagram Sorter" should {

    val csv = mock[BinarySearchCSV]
    val sorter = new AnagramSorter(csv)

    csv.find("a") returns Some(1L)
    csv.find("b") returns Some(2L)
    csv.find("c") returns Some(3L)
    csv.find("ab") returns Some(10L)
    csv.find("bc") returns Some(100L)

    "sort by the shortest number of words then minimum csv unigram value, descending and stable" in {
      sorter.sort(List("a b c", "a bc", "ab c")) must_== List("ab c", "a bc", "a b c")
    }

  }

}
