import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


//class SpacedStubApp extends optional.Application {
//  var spaced: Option[String] = _
//  var unspaced: Option[String] = _
//  var args: List[String] = _
//  
//  def main (switchlikethis: Option[String], switch_like_this: Option[String]) = {
//    spaced = switch_like_this
//    unspaced = switchlikethis
//    args = getArgs()
//  }
//}
//
//
//class SpacedTestSuite extends FunSuite with ShouldMatchers {
//  var app: SpacedStubApp = _
//  
//  def generally (commandLine: Array[String], desiredSpaced: Option[String], desiredUnspaced: Option[String]) = {
//    app = new SpacedStubApp
//    app.main(commandLine)
//    app.args should equal (List())
//    app.spaced should equal (desiredSpaced)
//    app.unspaced should equal (desiredUnspaced)
//    app = new SpacedStubApp
//    app.main (Array.concat (commandLine, Array("trailing")))
//    app.args should equal (List("trailing"))
//    app.spaced should equal (desiredSpaced)
//    app.unspaced should equal (desiredUnspaced)
//    app = new SpacedStubApp
//    app.main (Array.concat (Array("leading"), commandLine))
//    app.args should equal (List("leading"))
//    app.spaced should equal (desiredSpaced)
//    app.unspaced should equal (desiredUnspaced)
//  }
//  
//  test ("no switches works") {
//    generally (Array(), None, None)
//  }
//  
//  test ("unspaced switch only works") {
//    generally (Array ("--switchlikethis", "cabbage"), None, Some("cabbage"))
//  }
//  
//  test ("underscored switch only works") {
//    generally (Array ("--switch_like_this", "fruit"), Some("fruit"), None)
//  }
//  
//  test ("unspaced and underscored switches works") {
//    generally (Array ("--switchlikethis", "blaze", "--switch_like_this", "pool"), Some("pool"), Some("blaze"))
//  }
//  
//  test ("hyphenated switch only works") {
//    generally (Array ("--switch-like-this", "vegetable"), Some("vegetable"), None)
//  }
//  
//  test ("hyphenated and underscored switches works") {
//    generally (Array ("--switchlikethis", "blaze", "--switch-like-this", "onion"), Some("onion"), Some("blaze"))
//  }
//}

