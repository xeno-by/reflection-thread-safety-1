package optional

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.{ Matcher, MatchResult }

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

class ApplicationTraitTestSuite extends FunSuite with ShouldMatchers {
  object Test1 extends Application {
    def main(pos1: Int, pos2: String) {
      pos1 should be (1)
      pos2 should be ("2")
    }
  }
  test("positional agruments only") {
    val args = Array("1", "2")
    Test1.main(args)
  }
  
  class Test2(ev1: Option[Int], ev2: Option[String]) extends Application {
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
  
  class Test3(ev1: Int, ev2: String) extends Application {
    def main(p1: Int = 5, p2: String = "hello") {
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
  
  class Test4(ev1: Boolean, ev2: Boolean) extends Application {
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
  
  class Test5(ev1: Option[Boolean]) extends Application {
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
  
}