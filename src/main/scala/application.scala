package optional

import java.io.File.separator
import collection.mutable.HashSet

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

import org.apache.commons.{ cli => acli }

case class DesignError(msg: String) extends Error(msg)
case class UsageError(msg: String) extends RuntimeException(msg)

object Util {
  
  def cond[T](x: T)(f: PartialFunction[T, Boolean]) = (f isDefinedAt x) && f(x)
  def condOpt[T,U](x: T)(f: PartialFunction[T, U]): Option[U] = if (f isDefinedAt x) Some(f(x)) else None
  
  def stringForType(tpe: Type): String = tpe.typeSymbol.name.decoded
  
  def typeArgs(tpe: Type) = tpe match {
    case TypeRef(_, _, args) => args
    case _ => Nil
  }
}
import Util._

private class TypeExtractor(val t: Type) {
  def unapply(x: Any): Option[Type] = x match {
    case tpe: Type if tpe =:= t => Some(t)
    case _ => None
  }
}

private object BooleanType extends TypeExtractor(typeOf[Boolean])
private object StringType extends TypeExtractor(typeOf[String])
private object ArrayStringType extends TypeExtractor(typeOf[Array[String]])

object OptionType {
  def unapply(x: Any): Option[Type] = x match {
    case tpe: Type if tpe.erasure =:= typeOf[Option[Any]].erasure => {
      val arg = typeArgs(tpe).head
      Some(arg)
    }
    case _ => None
  }
}

object MainArg {
  def apply(term: TermSymbol, index: Int): MainArg = {
    val tpe = term.typeSignature
    tpe match {
      case BooleanType(_) => BoolArg(term, index)
      case OptionType(_)  => OptionArg(term, index)
      case _ => if (term.isParamWithDefault) {
        ArgWithDefault(term, index)
      } else {
        PositionalArg(term, index)
      }
    }    
  } 
}

sealed trait MainArg {
  def term: TermSymbol
  final def name: String = term.name.decoded
  def tpe: Type
  def isOptional: Boolean
  def usage: String
  def index: Int
  
  def =:=(other: MainArg): Boolean = name == other.name && 
                                     tpe =:= other.tpe &&
                                     isOptional == other.isOptional &&
                                     index == other.index
}

/**
 * An argument that can be specified by name
 */
sealed trait NamedArg extends MainArg {
  def cliOption: acli.Option
}

/**
 * An argument with a default value
 */
sealed trait DefaultArg extends NamedArg {
  def defaultValue(m: InstanceMirror): Any
}

/**
 * Definition for an optional argument
 * 
 * @name the name of the argument
 * @tpe the type of the argument without the wrapping Option, e.g. Int for Option[Int]
 * @originalType the type of the argument as defined on the main method
 */
case class OptionArg(term: TermSymbol, index: Int) extends DefaultArg {
  val tpe = term.typeSignature match {
    case OptionType(t) => t
  }
  def isOptional = true
  def usage = "[--%s %s]".format(name, stringForType(tpe))
  val cliOption = new acli.Option(name, true, "TBD")
  def defaultValue(m: InstanceMirror) = None
}

case class ArgWithDefault(term: TermSymbol, index: Int) extends DefaultArg {
  val tpe = term.typeSignature
  def isOptional = false
  def usage = "<%s: %s>".format(name, stringForType(tpe))
  
  val cliOption = new acli.Option(name, true, usage)
  
  def defaultValue(m: InstanceMirror): Any = {
    val baseMethodSym = term.owner
    val methodName = "%s$default$%d".format(baseMethodSym.name.decoded, index + 1)
    val termName = newTermName(methodName)
    val methSym = m.symbol.toType.member(termName).asMethod
    val methMirror = m.reflectMethod(methSym)
    methMirror()
  }
}

case class PositionalArg(term: TermSymbol, index: Int) extends MainArg {
  val tpe = term.typeSignature
  def isOptional = false
  def usage = "<%s>".format(stringForType(tpe))
}

case class BoolArg(term: TermSymbol, index: Int) extends DefaultArg {
  val tpe = typeOf[Boolean]
  def isOptional = true
  def usage = "[--%s]".format(name)
  val cliOption = new acli.Option(name, false, "include if true, omit if false")
  
  def defaultValue(m: InstanceMirror) = false
}


object Application {
  
  def isEligibleMain(m: MethodSymbol) = m.name.decoded == "main" && !isRealMain(m)
  
  def isRealMain(m: MethodSymbol) = {
    (m.name.decoded == "main") && (m.paramss.length == 1) && (m.paramss.head.length == 1) && (m.paramss.head.head.typeSignature == typeOf[Array[String]])
  }
  
  def findMainMethod(obj: AnyRef): MethodMirror = {
    val im = currentMirror.reflect(obj)
    val s = im.symbol
    val t = s.toType
    val candidateMethods = t.members.collect {
      case m if m.isMethod => m.asMethod
    }.filter(isEligibleMain(_)).toList
    
    candidateMethods match {
      case Nil => throw DesignError("No valid main method found!")
      case method :: Nil => im.reflectMethod(method)
      case _ => throw DesignError("%d potential main methods found!".format(candidateMethods.length))
    }
  }
  
  def extractArgs(meth: MethodSymbol) = meth.paramss.flatten.zipWithIndex.map { t =>
    val term = t._1.asTerm
    val pos = t._2
    MainArg(term, pos)
  }
  
  def signature(m: MethodSymbol): String = {
    m.name.decoded + m.paramss.flatMap { pl =>
      "(" + pl.flatMap { p => 
        p.name.decoded + ": " + p.typeSignature.toString
      } + ")"
    }
  }
  
  def makeRegistry = {
    val r = new ConverterRegistry[String]
    r.register(s => java.lang.Integer.parseInt(s))
    r.register(s => java.lang.Double.parseDouble(s))
    r.register(s => java.lang.Boolean.parseBoolean(s))
    r.register(s => scala.math.BigDecimal(s))
    r.register(s => scala.math.BigInt(s))
    r
  }
}



/**
 *  This trait automagically finds a main method on the object 
 *  which mixes this in and based on method names and types figures
 *  out the options it should be called with and takes care of parameter parsing
 */ 
//trait Application {
//  
//  /** These methods can be overridden to modify application behavior.
//   */
//
//  
//  /** The autogenerated usage message will usually suffice. */
//  protected def programName   = "program"
//  protected def usageMessage  = "Usage: %s %s".format(programName, mainArgs map (_.usage) mkString " ")
//
//  private def designError(msg: String) = throw DesignError(msg)
//  private def usageError(msg: String) = throw UsageError(msg)
//
//
//  lazy val registry = 
//  
//  
//  
//  /**
//   * Magic method to take a string and turn it into something of a given type.
//   */
//  private def coerceTo(name: String, tpe: Type)(value: String): Any = {
//    def fail      = designError("Could not create type '%s' from String".format(tpe))
//    def mismatch  = usageError("option --%s expects arg of type '%s' but was given '%s'".format(name, stringForType(tpe), value))
//    def surprise  = usageError("Unexpected type: %s (%s)".format(tpe, tpe.getClass))
//    
//    val r = tpe match {
//      case StringType(_)          => value
//      case ArrayStringType(_)     => value split separator
//      case OptionType(t)          => Some(coerceTo(name, t)(value))
//      case _ => registry.get(tpe) match {
//        case Some(f) => try {
//          f(value)
//        } catch {
//          case e: Exception => mismatch
//        }
//        case None => surprise
//      }
//    }
//    if (r == null) throw new Exception("arg %s of type %s and value of %s resulted in null".format(name, tpe.termSymbol.name.decoded, value))
//    r
//  }
//
//  def callWithOptions(): Unit = {
//    import opts._
//    def missing(s: String) = usageError("Missing required option '%s'".format(s))
//
//    // verify minimum quantity of positional arguments
//    if (args.size < posArgCount)
//      usageError("too few arguments: expected %d, got %d".format(posArgCount, args.size))
//    
//    // verify all required options are present
//    val missingArgs = reqArgs filter (x => !(options contains x.name) && !(x.name matches """^arg\d+$"""))
//    if (!missingArgs.isEmpty) {
//      val missingStr = missingArgs.map(a => "--" + a.name) mkString " "        
//      val s = if (missingArgs.size == 1) "" else "s"
//      
//      usageError("missing required option%s: %s".format(s, missingStr))
//    }
//    
//    // verify all options used are legitimate options
//    val illegitimateArgs = options.keySet filter (n => mainArgs forall (_.name != n))
//    if (!illegitimateArgs.isEmpty) {
//      val illegitimateStr = illegitimateArgs map ("--" + _) mkString " "
//      val s = if (illegitimateArgs.size == 1) "" else "s"
//      
//      usageError("invalid option%s found: %s".format(s, illegitimateStr))
//    }
//    
//    def determineValue(ma: MainArg): Any = {
//      def isPresent = options.contains(ma.name)
//      
//      ma match {
//        case PosArg(_, pos) => coerceTo(ma.name, ma.tpe)(args(pos))
//        case BoolArg(_, _) => isPresent
//        case ReqArg(_, _) => if (isPresent) coerceTo(ma.name, ma.tpe)(options(ma.name)) else missing(ma.name)
//        case OptArg(_, _) => if (isPresent) Some(coerceTo(ma.name, ma.tpe)(options(ma.name))) else None
//      }
//    }
//    
//    val mainMirror = {
//      val im = currentMirror.reflect(this)
//      im.reflectMethod(mainMethod)
//    }
//    val argArray = (mainArgs map determineValue).toArray
//    mainMirror(argArray: _*)
//  }
//  
//
//  
//  private def produceArguments(cmdline: Array[String]): (MethodSymbol, Array[Any]) = {
//
//    val userMain = mainMethod
//    val methodArgs = extractMethodArgs(userMain)
//    
//  }
//  
//  
//  def main(cmdline: Array[String]) {
//
//    
//    
//    try {
//      
//
//      
//      _opts = Options.parse(scala.collection.immutable.HashMap.newBuilder ++= mainArgs.map(x => (x.name, x)) result,
//                            argInfos, cmdline: _*)
//      callWithOptions()
//    } catch {
//      case UsageError(msg) =>
//        println("Error: " + msg)
//        println(usageMessage)
//    }
//  }
//}
