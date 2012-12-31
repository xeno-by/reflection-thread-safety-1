package optional

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.{ Matcher, MatchResult }

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

import org.apache.commons.{ cli => acli }

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

  def termForFirstArg(methodName: String): TermSymbol = {
    val im = currentMirror.reflect(this)
    val clsSym = im.symbol
    val clsTpe = clsSym.toType
    val mthSym = clsTpe.member(newTermName(methodName)).asMethod
    val param = mthSym.paramss.head.head
    param.asTerm 
  }
  
  test("test option type extractor successfully matches using typeOf on an Int") {
    val tpe = typeOf[Option[Int]]
    tpe match {
      case OptionType(t) => t should equivTo (typeOf[Int])
    }
  }
  
  def optionalIntMethod(arg: Option[Int]) = ???
  test("test option type extractor successfully matches using an extracted type on an Int") {
    val term = termForFirstArg("optionalIntMethod")
    term.typeSignature match {
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
    val term = termForFirstArg("optionalStringMethod")
    term.typeSignature match {
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
    val term = termForFirstArg("optionalBigDecimalMethod")
    term.typeSignature match {
      case OptionType(t) => t should equivTo (typeOf[scala.math.BigDecimal])
    }
  }
  
  def checkArgConstruction[T <: MainArg](methName: String, ef: TermSymbol => T): (T, T) = {
    val term = termForFirstArg(methName)
    val arg = MainArg(term, 0)
    val expected = ef(term)
    arg should equivTo (expected)
    (arg.asInstanceOf[T], expected)
  }
  
  test("Making an optional Int argument using an extracted type") {
    checkArgConstruction("optionalIntMethod", term => OptionArg(term, 0))
  }
  test("Obtaining optional int default value") {
    val (arg, _) = checkArgConstruction("optionalIntMethod", term => OptionArg(term, 0))
    val im = currentMirror.reflect(this)
    arg.defaultValue(im) should be (None) 
  }
  test("Testing optional int argument parsing") {
    val (arg, _) = checkArgConstruction("optionalIntMethod", term => OptionArg(term, 0))
    val parser = new acli.PosixParser()
    val cmdline = Array("-%s".format(arg.name), "5")
    val options = new acli.Options
    options.addOption(arg.cliOption)
    val r = parser.parse(options, cmdline)
    r.getOptionValue("arg") should be ("5")
  }
  
  test("Making an optional String argument using an extracted type") {
    checkArgConstruction("optionalStringMethod", term => OptionArg(term, 0))
  }
  test("Obtaining optional string default value") {
    val (arg, _) = checkArgConstruction("optionalStringMethod", term => OptionArg(term, 0))
    val im = currentMirror.reflect(this)
    arg.defaultValue(im) should be (None) 
  }
  
  test("Making an optional BigDecimal argument using an extracted type") {
    checkArgConstruction("optionalBigDecimalMethod", term => OptionArg(term, 0))
  }
  test("Obtaining optional big decimal default value") {
    val (arg, _) = checkArgConstruction("optionalBigDecimalMethod", term => OptionArg(term, 0))
    val im = currentMirror.reflect(this)
    arg.defaultValue(im) should be (None) 
  }  
  
  def optionalBooleanMethod(arg: Option[Boolean]) = ???
  test("Making an option boolean argument") {
    checkArgConstruction("optionalBooleanMethod", term => OptionArg(term, 0))
  }
  test("Obtaining optional boolean default value") {
    val (arg, _) = checkArgConstruction("optionalBooleanMethod", term => OptionArg(term, 0))
    val im = currentMirror.reflect(this)
    arg.defaultValue(im) should be (None) 
  }  
  
  def intWithDefaultMethod(arg: Int = 5) = ???
  test("Making an Int argument with a default value") {
    checkArgConstruction("intWithDefaultMethod", term => ArgWithDefault(term, 0))
  }
  test("Obtaining default int value") {
    val (arg, _) = checkArgConstruction("intWithDefaultMethod", term => ArgWithDefault(term, 0))
    val im = currentMirror.reflect(this)
    arg.defaultValue(im) should be (5) 
  }
  
  def stringWithDefaultMethod(arg: String = "hello") = ???
  test("Making a string argument with a default value") {
    checkArgConstruction("stringWithDefaultMethod", term => ArgWithDefault(term, 0))
  }
  test("Obtaining default string value") {
    val (arg, _) = checkArgConstruction("stringWithDefaultMethod", term => ArgWithDefault(term, 0))
    val im = currentMirror.reflect(this)
    arg.defaultValue(im) should be ("hello") 
  }
  
  def bigdecWithDefaultMethod(arg: scala.math.BigDecimal = scala.math.BigDecimal("42")) = ???
  test("Making a BigDecimal argument with a default value") {
    checkArgConstruction("bigdecWithDefaultMethod", term => ArgWithDefault(term, 0))
  }
  test("Obtaining default big decimal value") {
    val (arg, _) = checkArgConstruction("bigdecWithDefaultMethod", term => ArgWithDefault(term, 0))
    val im = currentMirror.reflect(this)
    arg.defaultValue(im) should be (scala.math.BigDecimal("42")) 
  }  
    
  def booleanMethod(arg1: Boolean) = ???
  test("Making a boolean argument using reflection") {
    checkArgConstruction("booleanMethod", term => BoolArg(term, 0))
  }
  test("Obtaining default boolean value") {
    val (arg, _) = checkArgConstruction("booleanMethod", term => BoolArg(term, 0))
    val im = currentMirror.reflect(this)
    arg.defaultValue(im) should be (false) 
  }  
  
  def intMethod(arg: Int) = ???
  test("Making a positional int argument") {
    checkArgConstruction("intMethod", term => PositionalArg(term, 0))
  }
  
  def stringMethod(arg: String) = ???
  test("Making a positional string argument") {
    checkArgConstruction("stringMethod", term => PositionalArg(term, 0))
  }
  
  def bigDecimalMethod(arg: scala.math.BigDecimal) = ???
  test("Making a positional big decimal argument") {
    checkArgConstruction("bigDecimalMethod", term => PositionalArg(term, 0))
  }
}