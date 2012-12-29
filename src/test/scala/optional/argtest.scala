package optional

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.{ Matcher, MatchResult }

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

class ArgTestSuite extends FunSuite with ShouldMatchers {
  def equivTo(right: Type) = Matcher { (left: Type) =>
    MatchResult(left =:= right, 
        "'%s' and '%s' are not equivalent".format(left.toString, right.toString), 
        "'%s' and '%s' are equivalent".format(left.toString, right,toString))
  }
  def equivTo(right: MainArg) = Matcher { (left: MainArg) =>
    MatchResult(left =:= right,
        "'%s' and '%s' are not equivalent".format(left.toString, right.toString), 
        "'%s' and '%s' are equivalent".format(left.toString, right,toString))
  }

  def typeOfFirstArg(methodName: String): Type = {
    val im = currentMirror.reflect(this)
    val clsSym = im.symbol
    val clsTpe = clsSym.toType
    val mthSym = clsTpe.member(newTermName(methodName)).asMethod
    val param = mthSym.paramss.head.head
    param.typeSignature    
  }
  
  test("test option type extractor successfully matches using typeOf on an Int") {
    val tpe = typeOf[Option[Int]]
    tpe match {
      case OptionType(t) => t should equivTo (typeOf[Int])
    }
  }
  
  def optionalIntMethod(arg: Option[Int]) = ???
  test("test option type extractor successfully matches using an extracted type on an Int") {
    val tpe = typeOfFirstArg("optionalIntMethod")
    tpe match {
      case OptionType(t) => t should equivTo (typeOf[Int])
    }
  }
  
  test("test option type extractor successfully matches using typeOf on a String") {
    val tpe = typeOf[Option[String]]
    tpe match {
      case OptionType(t) => t should equivTo (typeOf[String])
    }
  }
  
  def optionalStringMethod(arg: Option[String]) = ???
  test("test option type extractor successfully matches an extracted type on a String") {
    val tpe = typeOfFirstArg("optionalStringMethod")
    tpe match {
      case OptionType(t) => t should equivTo (typeOf[String])
    }
  }
  
  test("test option type extractor successfully matches using typeOf on a BigDecimal") {
    val tpe = typeOf[Option[scala.math.BigDecimal]]
    tpe match {
      case OptionType(t) => t should equivTo (typeOf[scala.math.BigDecimal])
    }
  }
  
  def optionalBigDecimalMethod(arg: Option[scala.math.BigDecimal]) = ???
  test("test option type extractor successfully matches an extracted type on a BigDecimal") {
    val tpe = typeOfFirstArg("optionalBigDecimalMethod")
    tpe match {
      case OptionType(t) => t should equivTo (typeOf[scala.math.BigDecimal])
    }
  }
  
//1. case class OptArg(name: String, tpe: Type, originalType: Type) extends MainArg
  test("Making an optional Int argument using typeOf") {
    val origType = typeOf[Option[Int]]
    val arg = MainArg("test", origType)
    arg should equivTo (OptArg("test", typeOf[Int]))
  }
  
  test("Making an optional Int argument using an extracted type") {
    val origType = typeOfFirstArg("optionalIntMethod")
    val arg = MainArg("test", origType)
    arg should equivTo (OptArg("test", typeOf[Int]))
  }
  
  test("Making an optional String argument using typeOf") {
    val origType = typeOf[Option[String]]
    val arg = MainArg("test", origType)
    arg should equivTo (OptArg("test", typeOf[String]))
  }
  
  test("Making an optional String argument using an extracted type") {
    val origType = typeOfFirstArg("optionalStringMethod")
    val arg = MainArg("test", origType)
    arg should equivTo (OptArg("test", typeOf[String]))
  }
  
  test("Making an optional BigDecimal argument using typeOf") {
    val origType = typeOf[Option[scala.math.BigDecimal]]
    val arg = MainArg("test", origType)
    arg should equivTo (OptArg("test", typeOf[scala.math.BigDecimal]))
  }
  
  test("Making an optional BigDecimal argument using an extracted type") {
    val origType = typeOfFirstArg("optionalBigDecimalMethod")
    val arg = MainArg("test", origType)
    arg should equivTo (OptArg("test", typeOf[scala.math.BigDecimal]))
  }
  
//2. case class ReqArg(name: String, tpe: Type) extends MainArg  
  
//3. case class PosArg(name: String, tpe: Type, override val pos: Int) extends MainArg  
//4. case class BoolArg(name: String) extends MainArg  
  test("Making a boolean argument using typeOf[Boolean]") {
    val arg = MainArg("test", typeOf[Boolean])
    arg should equivTo (BoolArg("test"))
  }
  
  def booleanMethod(arg1: Boolean) = ???
  test("Making a boolean argument using reflection") {
    val pt = typeOfFirstArg("booleanMethod")
    val arg = MainArg("test", pt)
    arg should equivTo (BoolArg("test"))
  }
  
}