package optional

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.{ Matcher, MatchResult }

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

class ApplicationTraitTestSuite extends FunSuite with ShouldMatchers {
  trait TestApp extends optional.Application {
    protected override def catchUsageError = false
  }
  
  
  class Test1(ev1: Int = 1, ev2: String = "2") extends TestApp {
    def main(pos1: Int, pos2: String) {
      pos1 should be (1)
      pos2 should be ("2")
    }
  }
  test("positional agruments only") {
    val Test1 = new Test1(1, "2")
    val args = Array("1", "2")
    Test1.main(args)
  }
  test("insuffient positional arguments") {
    val Test1 = new Test1(1, "2")
    val args = Array("1")
    val thrown = evaluating {
      Test1.main(args)
    } should produce [UsageError]
    thrown.msg should be ("Received 1 positional arguments and expected 2")
  }
  test("malformed positional argument") {
    val Test1 = new Test1(1, "2")
    val args = Array("one", "2")
    val thrown = evaluating {
      Test1.main(args)
    } should produce [UsageError]
    //thrown.msg should be ("Malformed argument value \"one\" for pos1 of type Int:\nthe value \"one\" could not be converted into an integer\nUsage: <Int>")
  }
  test("test program name") {
    val Test1 = new Test1(1, "2")
    Test1.programName should be ("Test1")
  }
  
  def checkUsageMessageHeader(expected: String, app: TestApp) {
    val mm = Application.findMainMethod(app)
    val (posArgs, namedArgs) = MainArg.splitArgs(Application.extractArgs(mm.symbol))
    val r = app.usageMessageHeader(posArgs, namedArgs)
    r should be (expected)
  }
  
  test("test usage message header with just positional arguments") {
    checkUsageMessageHeader("Test1: <pos1: Int> <pos2: String>", new Test1)
  }
  
  class Test2(ev1: Option[Int] = None, ev2: Option[String] = None) extends TestApp {
    def main(opt1: Option[Int], opt2: Option[String]) {
      opt1 should be (ev1)
      opt2 should be (ev2)
    }
  }
  test("option arguments only w/o defined defaults and all options specified") {
    val Test2 = new Test2(Some(1), Some("2"))
    val args = Array("-opt1", "1", "-opt2", "2")
    Test2.main(args)
  }
  test("option arguments w/o defaults and no options specified") {
    val Test2 = new Test2(None, None)
    val args = Array[String]()
    Test2.main(args)
  }
  test("option arguments w/o defaults, one specified, one not") {
    val Test2 = new Test2(None, Some("2"))
    val args = Array("-opt2", "2")
    Test2.main(args)
  }
  test("usage message header for just Option arguments") {
    checkUsageMessageHeader("Test2: [options]", new Test2)
  }
  
  class Test3(ev1: Int = 5, ev2: String = "hello") extends TestApp {
    def main(p1: Int = ev1, p2: String = ev2) {
      p1 should be (ev1)
      p2 should be (ev2)
    }
  }
  test("default arguments with all present") {
    val Test3 = new Test3(1, "2")
    val args = Array("-p1", "1", "-p2", "2")
    Test3.main(args)
  }
  test("default arguments with none present") {
    val Test3 = new Test3(5, "hello")
    val args = Array[String]()
    Test3.main(args)
  }
  test("default arguments with one present, one missing") {
    val Test3 = new Test3(5, "two")
    val args = Array("-p2", "two")
    Test3.main(args)
  }
  test("usage message header for just arguments with defaults") {
    checkUsageMessageHeader("Test3: [options]", new Test3)
  }
  
  class Test4(ev1: Boolean = false, ev2: Boolean = false) extends TestApp {
    def main(p1: Boolean, p2: Boolean) {
      p1 should be (ev1)
      p2 should be (ev2)
    }
  }
  test("boolean flags with both present") {
    val Test4 = new Test4(true, true)
    val args = Array("-p1", "-p2")
    Test4.main(args)
  }
  test("boolean flags with both missing") {
    val Test4 = new Test4(false, false)
    val args = Array[String]()
    Test4.main(args)
  }
  test("boolean flags with one missing, one present") {
    val Test4 = new Test4(false, true)
    val args = Array[String]("-p2")
    Test4.main(args)
  }
  test("usage message beader for just boolean arguments") {
    checkUsageMessageHeader("Test4: [options]", new Test4)
  }
  
  class Test5(ev1: Option[Boolean]) extends TestApp {
    def main(p1: Option[Boolean]) {
      p1 should be (ev1)
    }
  }
  test("option arg with a type of boolean with arg specified") {
    val Test5 = new Test5(Some(false))
    val args = Array("-p1", "false")
    Test5.main(args)
  }
  test("option arg with type of boolean with arg unspecified") {
    val Test5 = new Test5(None)
    val args = Array[String]()
    Test5.main(args)
  }
  
  class Test6(ev1: Int = 1, ev2: Boolean = false) extends TestApp {
    def main(pos1: Int, b1: Boolean) {
      pos1 should be (ev1)
      b1 should be (ev2)
    }
  }
  test("usage header with one positional and one boolean") {
    checkUsageMessageHeader("Test6: [options] <pos1: Int>", new Test6)
  }
  
}