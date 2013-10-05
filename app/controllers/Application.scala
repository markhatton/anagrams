package controllers

import play.api._
import play.api.libs.json.Json
import play.api.mvc._
import services._
import scala.io.Source
import java.io.File

object Application extends Controller {

  private def loadCsv(name: String, filename: String) = {
    val unigramsFile = new File(filename)
    if (!unigramsFile.exists()) sys error s"unable to load input CSV file: $filename"

    Logger.info(s"Loading $name from file: $filename")
    new BinarySearchCSV(unigramsFile)
  }

  val cppExecutable = System.getProperty("anagrams-cpp", "/Users/markhatton/Src/Mark/anagrams-cpp/anagrams")

  val unigramsFile = System.getProperty("unigrams", "/Users/markhatton/unigrams")

  val bigramsFile = System.getProperty("bigrams", "/Users/markhatton/bigrams")

  val unigrams = loadCsv("unigrams", unigramsFile)

  val bigrams = loadCsv("bigrams", bigramsFile)

  val anagramSolver: AnagramSolver = System.getProperty("solver", "native") match {
    case "external" =>
      Logger.info(s"Using external anagrams solver: $cppExecutable")
      ExternalAnagramSolver

    case "native" =>
      Logger.info(s"Using native anagrams solver")

      val dictionaryFile = System.getProperty("dictionary", "/usr/share/dict/words")

      if (!new File(dictionaryFile).exists) sys error s"unable to load input dictionary file: $dictionaryFile"
      val dictionary = {
        Logger.info(s"Loading dictionary from file: $dictionaryFile")
        val dictionary = Source.fromFile(dictionaryFile).getLines()
        dictionary.filter{s => s.length > ShortWords.maxLength || ShortWords.toSet.contains(s)}.toSet
      }
      new NativeAnagramSolver(dictionary, unigrams)
  }

  val sorter = {
    val presenter = new AnagramPresenter(unigrams, bigrams)
    new AnagramSorter(unigrams, presenter)
  }
  
  
  private val BotUserAgent = """^.*\([^;]*;.*[A-Za-z0-9]+[Bb][Oo][Tt]/.*""".r

  private def showIndex = Ok(views.html.index())

  def index = Action { request =>
    request.headers.get("User-Agent") match {
      case Some(BotUserAgent()) =>
        request.getQueryString("s") match {
          case Some(s) if s.trim.nonEmpty =>
            Ok(views.html.solve(anagramSolver, sorter, s))  // non-JS route for bots
          case _ =>
            showIndex
        }
      case _ =>
        showIndex
    }
  }

  def solve = Action { request =>
    request match {
      case Accepts.Html() =>
        request.getQueryString("s") match {
          case Some(s) if s.trim.nonEmpty =>
            Redirect(s"/?s=$s", 301)
          case _ =>
            Redirect("/", 301)
        }
      case _ =>
        request.getQueryString("s") match {
          case Some(s) if s.trim.nonEmpty =>
            Ok(Json.toJson(Map("solutions"->sorter.sort(anagramSolver.solve(s)))))
          case _ =>
            BadRequest
        }
    }
  }

}