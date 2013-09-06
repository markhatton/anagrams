/*
 * Copyright (c) 2013 Mark Hatton
 */
package services

import org.specs2.mutable.SpecificationWithJUnit
import java.io.File

class BinarySearchCSVSpec extends SpecificationWithJUnit {

  "A Binary Search CSV" should {

    val csv = {
      val fileOpt = Option(this.getClass.getClassLoader.getResource("unigrams.csv"))
      new BinarySearchCSV(new File(fileOpt.getOrElse(sys error "unable to load input CSV").getFile))
    }

    "resolve CSV values" in {
      csv.find("acabados") must_== Some(1172)
      csv.find("ac") must_== Some(1815808L)
      csv.find("abzwecken") must_== Some(40L) // first line
      csv.find("zoology") must_== Some(123L) // last line
    }

    "return None if no such key" in {
      csv.find("aaa") must_== None // before start of file
      csv.find("acaac") must_== None // in-between keys
      csv.find("!!!") must_== None // after end of file
    }

    "brute force test" in {
      csv.find("abzwecken") must_== Some(40)
      csv.find("abzweigen") must_== Some(151)
      csv.find("abzweigende") must_== Some(47)
      csv.find("abzweigenden") must_== Some(78)
      csv.find("abzweigt") must_== Some(102)
      csv.find("abzweigte") must_== Some(47)
      csv.find("abzwingen") must_== Some(143)
      csv.find("abzyme") must_== Some(706)
      csv.find("abzymes") must_== Some(1567)
      csv.find("ac") must_== Some(1815808)
      csv.find("aca") must_== Some(30021)
      csv.find("acaa") must_== Some(260)
      csv.find("acaaa") must_== Some(56)
      csv.find("acaadamy") must_== Some(131)
      csv.find("acaademy") must_== Some(80)
      csv.find("acaaemic") must_== Some(56)
      csv.find("acaan") must_== Some(80)
      csv.find("acab") must_== Some(333)
      csv.find("acaba") must_== Some(13458)
      csv.find("acababa") must_== Some(2710)
      csv.find("acababan") must_== Some(675)
      csv.find("acabad") must_== Some(236)
      csv.find("acabada") must_== Some(2530)
      csv.find("acabadamente") must_== Some(58)
      csv.find("acabadas") must_== Some(631)
      csv.find("acabado") must_== Some(8146)
      csv.find("acabados") must_== Some(1172)
      csv.find("acaballo") must_== Some(48)
      csv.find("acabam") must_== Some(460)
      csv.find("acabament") must_== Some(52)
      csv.find("acabamento") must_== Some(898)
      csv.find("acabamiento") must_== Some(255)
      csv.find("acabamos") must_== Some(1995)
      csv.find("acaban") must_== Some(3434)
      csv.find("acabando") must_== Some(1682)
      csv.find("acabandose") must_== Some(54)
      csv.find("acabao") must_== Some(125)
      csv.find("acabar") must_== Some(12473)
      csv.find("acabara") must_== Some(1090)
      csv.find("acabaram") must_== Some(365)
      csv.find("acabaran") must_== Some(286)
      csv.find("acabaras") must_== Some(54)
      csv.find("acabard") must_== Some(56)
      csv.find("acabare") must_== Some(108)
      csv.find("acabarem") must_== Some(88)
      csv.find("acabaremos") must_== Some(133)
      csv.find("acabaria") must_== Some(244)
      csv.find("acabarla") must_== Some(128)
      csv.find("acabarle") must_== Some(57)
      csv.find("zoology") must_== Some(123)
    }

  }

}
