/*
 * Copyright (c) 2013 zeebox
 */
package services

import io.Source
import controllers.Application
import play.api.Logger

object ExternalAnagramSolver extends AnagramSolver {

  def solve(s: String, limit: Int, timeoutMillis: Int): List[String] = {
    val cmd = Array(
      Application.cppExecutable,
      "-u",
      Application.unigramsFile,
      "-l",
      limit.toString,
      "-t",
      (timeoutMillis / 1000).toString,
      "--",
      s
    )
    val process = Runtime.getRuntime.exec(cmd)
    val solutions = Source.fromInputStream(process.getInputStream).getLines().toList
    Logger.info(s"Anagram [$s], found ${solutions.length} solutions")
    solutions
  }

}
