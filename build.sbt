import play.Project._

name := "anagrams"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
  )

libraryDependencies ++= Seq(
  "org.ardverk" % "patricia-trie" % "0.6",
  "org.mockito" % "mockito-all" % "1.9.5" % "test"
  )

resolvers += "Ardverk" at "http://mvn.ardverk.org/repository/release/"

playScalaSettings
