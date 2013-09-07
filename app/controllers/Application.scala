package controllers

import play.api._
import play.api.mvc._
import services._
import scala.io.Source
import java.io.File

object Application extends Controller {

  private def loadCsv(name: String) = {
    val filename = Option(System.getProperty("name")).getOrElse(s"/Users/markhatton/$name")
    val unigramsFile = new File(filename)
    if (!unigramsFile.exists()) sys error s"unable to load input CSV file: $filename"

    Logger.info(s"Loading $name from file: $filename")
    new BinarySearchCSV(unigramsFile)
  }

  val unigrams = loadCsv("unigrams")

  val bigrams = loadCsv("bigrams")

  val anagramSolver = {
    val filename = Option(System.getProperty("dictionary")).getOrElse("/usr/share/dict/words")

    if (!new File(filename).exists) sys error s"unable to load input dictionary file: $filename"
    val dictionary = {
      Logger.info(s"Loading dictionary from file: $filename")
      val dictionary = Source.fromFile(filename).getLines()
      dictionary.filter{s => s.length > ShortWords.maxLength || ShortWords.toSet.contains(s)}.toSet
    }
    new AnagramSolver(dictionary, unigrams)
  }


  val sorter = {
    val presenter = new AnagramPresenter(unigrams, bigrams)
    new AnagramSorter(unigrams, presenter)
  }

  def index = Action {
    Ok(views.html.index())
  }

  def solve = Action { request =>
    request.getQueryString("s") match {
      case None =>
        Redirect("/", 301)
      case Some(s) =>
        Ok(views.html.solve(anagramSolver, sorter, s))
    }
  }

}