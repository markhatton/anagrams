import play.Project._

name := "anagrams"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
  )     

playScalaSettings
