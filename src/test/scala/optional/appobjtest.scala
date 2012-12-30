package optional

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.{ Matcher, MatchResult }

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror


class ApplicationObjectTestSuite extends FunSuite with ShouldMatchers {
  object NoUserMain {
    def main(args: Array[String]): Unit = { }
  }
  test("isRealMain should return true on well formed real main") {
    val im = currentMirror.reflect(NoUserMain)
    val tpe = im.symbol.toType
    val ms = tpe.member(newTermName("main")).asMethod
    Application.isRealMain(ms) should be (true)
  }
  test("isEligleMain should return false on well formed real main") {
    val im = currentMirror.reflect(NoUserMain)
    val tpe = im.symbol.toType
    val ms = tpe.member(newTermName("main")).asMethod
    Application.isEligibleMain(ms) should be (false)
  }
  test("Find a user main method should result in a DesignError because it doesn't exist") {
    val thrown = evaluating {
      Application.findMainMethod(NoUserMain)
    } should produce [DesignError]
    thrown.msg should be ("No valid main method found!")
  }
  
  object NoMainMain {
    def main(a: Int, b: String): Unit = ???
  }
  test("isRealMain should return false on a user main") {
    val im = currentMirror.reflect(NoMainMain)
    val tpe = im.symbol.toType
    val ms = tpe.member(newTermName("main")).asMethod
    Application.isRealMain(ms) should be (false)
  }
  test("isEligleMain should return true on well formed user main") {
    val im = currentMirror.reflect(NoMainMain)
    val tpe = im.symbol.toType
    val ms = tpe.member(newTermName("main")).asMethod
    Application.isEligibleMain(ms) should be (true)
  }  
  test("Finding a user main method should succeed even if no real main method exists") {
    val mm = Application.findMainMethod(NoMainMain)
    
  }
  
  object TooManyMains {
    def main(args: Array[String]): Unit = {}
    def main(a: Int, b: String): Unit = ???
    def main(x: String, y: Option[String]): Unit = ???
  }
  test("Finding a user main should fail because there are two candidates") {
    val thrown = evaluating {
      Application.findMainMethod(TooManyMains)
    } should produce [DesignError]
    thrown.msg should be ("2 potential main methods found!")    
  }
  
  object GoodMains {
    def main(args: Array[String]): Unit = {}
    def main(a: Int, b: String): Unit = ???
  }
  test("Finding a user main method should succeed in a well-formed object") {
    val mm = Application.findMainMethod(GoodMains)
  }
  test("Testing argument extraction on a simple case") {
    val mm = Application.findMainMethod(GoodMains)
    val s = mm.symbol
    s.paramss.length should be (1)
    val fl = s.paramss.head
    fl.length should be (2)
    val at = fl(0)
    val bt = fl(1)
    val args = Application.extractArgs(s)
    args.length should be (2)
    val a = args(0)
    a.name should be ("a")
    a.isInstanceOf[PositionalArg] should be (true)
    val b = args(1)
    b.name should be ("b")
    b.isInstanceOf[PositionalArg] should be (true)
  }
  
  object ComplexMain {
    def main(a: Int, b: Boolean, c: String, d: Option[Int], e: String = "Hello"): Unit = {}
  }
  test("Testing argument extraction on a complex main") {
    val mm = Application.findMainMethod(ComplexMain)
    val args = Application.extractArgs(mm.symbol)
    args.length should be (5)
    args match {
      case List(a, b, c, d, e) => {
        a.name should be ("a")
        b.name should be ("b")
        c.name should be ("c")
        d.name should be ("d")
        e.name should be ("e")
        a.isInstanceOf[PositionalArg] should be (true)
        b.isInstanceOf[BoolArg] should be (true)
        c.isInstanceOf[PositionalArg] should be (true)
        d.isInstanceOf[OptionArg] should be (true)
        e.isInstanceOf[ArgWithDefault] should be (true)
      }
    }
  }
}