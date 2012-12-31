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
  
}