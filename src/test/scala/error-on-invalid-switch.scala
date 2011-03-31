import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class InvalidSwitchStubApp extends optional.Application {
  var parsedCommandLine: Option [(List[String], String, Option[String])] = None
  
  def main (compulsory: String, optional: Option[String]) {
    parsedCommandLine = Some ((getArgs(), compulsory, optional))
  }
}


class InvalidSwitchTestSuite extends FunSuite with ShouldMatchers {
  def switch (name: String, value: Option[String]): Array[String]
     = value match {
         case Some(v) => Array (name, v)
         case None    => Array()
       }

  def commandLine (compulsory: Option[String], optional: Option[String], banned: Option[String]): Array[String]
     = Array.concat (Array("begin"),
                     switch ("--compulsory", compulsory),
                     switch ("--optional", optional),
                     switch ("--banned", banned),
                     Array("end"))

  def disallow (compulsory: Option[String], optional: Option[String], banned: Option[String]) = {
    val app = new InvalidSwitchStubApp
    app.main (commandLine (compulsory, optional, banned))
    app.parsedCommandLine should equal (None)
  }
  
  def allow (compulsory: String, optional: Option[String]) = {
    val app = new InvalidSwitchStubApp
    app.main (commandLine (Some(compulsory), optional, None))
    app.parsedCommandLine should equal (Some ((List ("begin", "end"), compulsory, optional)))
  }
  
  test ("no switches is error") {
    disallow (None,               None,             None)
  }

  test ("optional switch only is error") {
    disallow (None,               Some("OPTIONAL"), None)
  }

  test ("no compulsory switch is error") {
    disallow (None,               Some("OPTIONAL"), Some("BANNED"))
  }

  test ("banned switch only is error") {
    disallow (None,               None,             Some("BANNED"))
  }

  test ("optional switch only missing switch is error") {
    disallow (Some("COMPULSORY"), None,             Some("BANNED"))
  }

  test ("all switches is error") {
    disallow (Some("COMPULSORY"), Some("OPTIONAL"), Some("BANNED"))
  }

  test ("banned switch only missing switch is allowed") {
    allow    (     "COMPULSORY",  Some("OPTIONAL"))
  }

  test ("compulsory switch only is allowed") {
    allow    (     "COMPULSORY",  None)
  }
}

