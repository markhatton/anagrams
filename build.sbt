import play.Project._

name := "anagrams"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
  )

libraryDependencies += "org.ardverk" % "patricia-trie" % "0.6"

resolvers += "Ardverk" at "http://mvn.ardverk.org/repository/release/"

playScalaSettings
