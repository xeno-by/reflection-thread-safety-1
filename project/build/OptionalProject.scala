import sbt._

class OptionalProject(info: ProjectInfo) extends DefaultProject(info) {  
  val paranamer  = "com.thoughtworks.paranamer" % "paranamer" % "2.2.1"
  val scalatest  = buildScalaVersion match {
    case "2.9.0" | "2.9.0-1" => "org.scalatest" % "scalatest_2.9.0" % "1.4.1"
    case "2.8.0" | "2.8.1"   => "org.scalatest" % "scalatest_2.8.1" % "1.5"
    case s => error("Unsupported version: " + s)
  }
}
