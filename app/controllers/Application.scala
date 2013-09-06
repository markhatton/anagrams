package controllers

import play.api._
import play.api.mvc._
import services.{AnagramSorter, BinarySearchCSV, AnagramSolver}
import scala.io.Source
import java.io.File

object Application extends Controller {

  lazy val anagramSolver = {
//    val dictFile = "/usr/share/dict/words"
    val dictFile = "/Users/markhatton/words"
    val dictionary = {
      val ss = Set("a", "i")
      Source.fromFile(dictFile).getLines().filterNot(s => s.length == 1 && !ss.contains(s)).toSet
    }
    new AnagramSolver(dictionary)
  }

  lazy val sorter = {
    val csvFile = new File("/Users/markhatton/unigrams")
    new AnagramSorter(new BinarySearchCSV(csvFile))
  }

  def index = Action {
    Ok(views.html.index())
  }

  def solve = Action { request =>
    request.getQueryString("s") match {
      case None =>
        BadRequest
      case Some(s) =>
        Ok(views.html.solve(anagramSolver, sorter, s))
    }
  }

}