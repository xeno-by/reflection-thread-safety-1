import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class BooleanStubApp extends optional.Application {
  var _option: Boolean = _
  var _args: List[String] = _
  
  def main (option: Boolean) = {
    _option = option
    _args = getArgs()
  }
}


class BooleanTestSuite extends FunSuite with ShouldMatchers {
  def generally (commandLine: Array[String], desiredOption: Boolean) {
    var app: BooleanStubApp = new BooleanStubApp
    app.main (commandLine)
    app._args should equal (List())
    app._option should equal (desiredOption)
    app = new BooleanStubApp
    app.main (Array.concat (commandLine, Array("trailing")))
    app._args should equal (List("trailing"))
    app._option should equal (desiredOption)
    app = new BooleanStubApp
    app.main (Array.concat (Array("leading"), commandLine))
    app._args should equal (List("leading"))
    app._option should equal (desiredOption)
  }
  
  test ("absent switch means false") {
    generally (Array(), false)
  }
  
  test ("switch if present means true and does not take an argument") {
    generally (Array("--option"), true)
  }
}

