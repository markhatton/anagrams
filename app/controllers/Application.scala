package controllers

import play.api._
import play.api.mvc._
import services.AnagramSolver
import scala.io.Source

object Application extends Controller {

  lazy val anagramSolver = {
    val dictionary = {
      val ss = Set("a", "i")
      Source.fromFile("/usr/share/dict/words").getLines().filterNot(s => s.length == 1 && !ss.contains(s)).toSet
    }
    new AnagramSolver(dictionary)
  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def solve = Action { request =>
    request.getQueryString("s") match {
      case None =>
        BadRequest
      case Some(s) =>
        Ok(views.html.solve(anagramSolver, s))
    }
  }

}