package controllers

import play.api._
import play.api.mvc._
import services.{ShortWords, AnagramSorter, BinarySearchCSV, AnagramSolver}
import scala.io.Source
import java.io.File

object Application extends Controller {

  val unigrams = {
    val filename = Option(System.getProperty("unigrams")).getOrElse("/Users/markhatton/unigrams")
    val unigramsFile = new File(filename)
    if (!unigramsFile.exists()) sys error s"unable to load input CSV file: $filename"

    Logger.info(s"Loading unigrams from file: $filename")
    new BinarySearchCSV(unigramsFile)
  }

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
    new AnagramSorter(unigrams)
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