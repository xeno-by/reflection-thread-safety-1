import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class StubApp extends optional.Application {
  var fish_ : Option[String] = _
  var fowl_ : String = _
  var args_ : List[String] = _

  def main (fish: Option[String], fowl: String) = {
    fish_ = fish
    fowl_ = fowl
    args_ = getArgs()
  }
}


class TestSuite extends FunSuite with ShouldMatchers {
  test ("absent optional arg is None") {
    val app = new StubApp
    app.main (Array ("--fowl", "turkey"))
    app.fish_ should equal (None)
  }
  
  test ("present optional arg is Some") {
    val app = new StubApp
    app.main (Array ("--fowl", "chicken", "--fish", "halibut"))
    app.fish_ should equal (Some("halibut"))
  }
  
  test ("present compulsory arg is passed") {
    val app = new StubApp
    app.main (Array ("--fowl", "chicken", "--fish", "halibut"))
    app.fowl_ should equal ("chicken")
  }
  
  test ("positional arguments are passed") {
    val app = new StubApp
    app.main (Array ("taxrises", "spiritlevel", "--fowl", "goose", "trickledown", "spendingcuts"))
    app.args_ should equal (List ("taxrises", "spiritlevel", "trickledown", "spendingcuts"))
  }
}


