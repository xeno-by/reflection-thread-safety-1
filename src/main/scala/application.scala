package optional

import java.io.File.separator
import collection.mutable.HashSet

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

case class DesignError(msg: String) extends Error(msg)
case class UsageError(msg: String) extends RuntimeException(msg)

object Util {  
  val Argument = """^arg(\d+)$""".r
  
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

private object OptionType {
  def unapply(x: Any): Option[Type] = x match {
    case tpe: Type if tpe.erasure =:= typeOf[Option[Any]].erasure => {
      val arg = typeArgs(tpe).head
      Some(arg)
    }
    case _ => None
  }
}

object MainArg {
  private val bt = typeOf[Boolean]
  def apply(name: String, tpe: Type): MainArg = tpe match {
    case BooleanType(_) => BoolArg(name)
    case OptionType(t)  => OptArg(name, t, tpe)
    case _              =>
      name match {      
        case Argument(num)  => PosArg(name, tpe, num.toInt)
        case _              => ReqArg(name, tpe)
      }
  }

  def unapply(x: Any): Option[(String, Type, Type)] = x match {
    case OptArg(name, tpe, originalType)  => Some(name, tpe, originalType)
    case BoolArg(name)                    => Some(name, bt, bt)
    case ReqArg(name, tpe)                => Some(name, tpe, tpe)
    case PosArg(name, tpe, num)           => Some(name, tpe, tpe)
  }
}

sealed abstract class MainArg {
  def name: String
  def tpe: Type
  def originalType: Type
  def isOptional: Boolean
  def usage: String
  
  def pos: Int = -1
  def isPositional = pos > -1
  def isBoolean = false
  
  def =:=(other: MainArg): Boolean = name == other.name && 
                                     tpe =:= other.tpe &&
                                     originalType =:= other.originalType &&
                                     isOptional == other.isOptional && 
                                     pos == other.pos
}

/**
 * Definition for an optional argument
 * 
 * @name the name of the argument
 * @tpe the type of the argument without the wrapping Option, e.g. Int for Option[Int]
 * @originalType the type of the argument as defined on the main method
 */
case class OptArg(name: String, tpe: Type, originalType: Type) extends MainArg {
  val isOptional = true
  def usage = "[--%s %s]".format(name, stringForType(tpe))
}

case class ReqArg(name: String, tpe: Type) extends MainArg {
  val originalType = tpe
  val isOptional = false
  def usage = "<%s: %s>".format(name, stringForType(tpe))
}

case class PosArg(name: String, tpe: Type, override val pos: Int) extends MainArg {
  val originalType = tpe
  val isOptional = false
  def usage = "<%s>".format(stringForType(tpe))
}

case class BoolArg(name: String) extends MainArg {
  override def isBoolean = true
  val tpe, originalType = typeOf[Boolean]
  val isOptional = true
  def usage = "[--%s]".format(name)
}

/**
 *  This trait automagically finds a main method on the object 
 *  which mixes this in and based on method names and types figures
 *  out the options it should be called with and takes care of parameter parsing
 */ 
trait Application {

  /** Public methods.
   */
  def getRawArgs()  = opts.rawArgs
  def getArgs()     = {
     val r = opts.args
     if (r eq null) throw new Exception("How did opts.args end up null?")
     r
  }
  
  /** These methods can be overridden to modify application behavior.
   */

  /** Override this if you want to restrict the search space of conversion methods. */
  protected def isConversionMethod(m: MethodSymbol) = true
  
  /** The autogenerated usage message will usually suffice. */
  protected def programName   = "program"
  protected def usageMessage  = "Usage: %s %s".format(programName, mainArgs map (_.usage) mkString " ")
  
  /** If you need to set up more argument info */
  protected def register(xs: ArgInfo*) { xs foreach (argInfos += _) }
  
  
  private def methods(f: MethodSymbol => Boolean): List[MethodSymbol] = {
    val im = currentMirror.reflect(this)
    val s = im.symbol
    val t = s.toType
    val methods = t.members.collect {
      case m if m.isMethod => m.asMethod
    }
    methods.filter(f).toList
  }
  
  private def signature(m: MethodSymbol): String = {
    m.name.decoded + m.paramss.flatMap { pl =>
      "(" + pl.flatMap { p => 
        p.name.decoded + ": " + p.typeSignature.toString
      } + ")"
    }
  }
  
  private def designError(msg: String) = throw DesignError(msg)
  private def usageError(msg: String) = throw UsageError(msg)

  private def isRealMain(m: MethodSymbol) = {
    (m.name.decoded == "main") && (m.paramss.length == 1) && (m.paramss.head.length == 1) && (m.paramss.head.head.typeSignature == typeOf[Array[String]])
  }
  
  private def isEligibleMain(m: MethodSymbol) = m.name.decoded == "main" && !isRealMain(m)
  
  private lazy val mainMethod = methods(isEligibleMain) match {
    case Nil      => designError("No eligible main method found")
    case List(x)  => x
    case xs       =>
      designError("You seem to have multiple main methods, signatures:\n%s" .
        format(xs map signature mkString "\n")
      )
  }
  
  private lazy val mainArgs: List[MainArg] = mainMethod.paramss.flatten.map { p =>
    val name = p.name.decoded
    val tpe = p.typeSignature
    MainArg(name, tpe)
  }
  private lazy val reqArgs          = mainArgs.filter(x => !x.isOptional)
  private def posArgCount           = mainArgs.filter(_.isPositional).size

  private val registry = new ConverterRegistry[String]
  registry.register(s => java.lang.Integer.parseInt(s))
  registry.register(s => java.lang.Double.parseDouble(s))
  registry.register(s => java.lang.Boolean.parseBoolean(s))
  registry.register(s => scala.math.BigDecimal(s))
  registry.register(s => scala.math.BigInt(s))
  
  
  
  /**
   * Magic method to take a string and turn it into something of a given type.
   */
  private def coerceTo(name: String, tpe: Type)(value: String): Any = {
    def fail      = designError("Could not create type '%s' from String".format(tpe))
    def mismatch  = usageError("option --%s expects arg of type '%s' but was given '%s'".format(name, stringForType(tpe), value))
    def surprise  = usageError("Unexpected type: %s (%s)".format(tpe, tpe.getClass))
    
    tpe match {
      case StringType(_)          => value
      case ArrayStringType(_)     => value split separator
      case OptionType(t)          => Some(coerceTo(name, t)(value))
      case _ => registry.get(tpe) match {
        case Some(f) => try {
          f(value)
        } catch {
          case e: Exception => mismatch
        }
        case None => surprise
      }
    }
  }

  private var _opts: Options = null
  lazy val opts = {
    if (_opts eq null) throw new Exception("somehow _opts ended up being null")
    _opts 
  }
  
  private val argInfos = new HashSet[ArgInfo]()

  def callWithOptions(): Unit = {
    import opts._
    def missing(s: String) = usageError("Missing required option '%s'".format(s))

    // verify minimum quantity of positional arguments
    if (args.size < posArgCount)
      usageError("too few arguments: expected %d, got %d".format(posArgCount, args.size))
    
    // verify all required options are present
    val missingArgs = reqArgs filter (x => !(options contains x.name) && !(x.name matches """^arg\d+$"""))
    if (!missingArgs.isEmpty) {
      val missingStr = missingArgs.map(a => "--" + a.name) mkString " "        
      val s = if (missingArgs.size == 1) "" else "s"
      
      usageError("missing required option%s: %s".format(s, missingStr))
    }
    
    // verify all options used are legitimate options
    val illegitimateArgs = options.keySet filter (n => mainArgs forall (_.name != n))
    if (!illegitimateArgs.isEmpty) {
      val illegitimateStr = illegitimateArgs map ("--" + _) mkString " "
      val s = if (illegitimateArgs.size == 1) "" else "s"
      
      usageError("invalid option%s found: %s".format(s, illegitimateStr))
    }
    
    def determineValue(ma: MainArg): Any = {
      val MainArg(name, _, tpe) = ma
      def isPresent = options contains name
      
      if (ma.isPositional)      coerceTo(name, tpe)(args(ma.pos - 1))
      else if (isPresent)       coerceTo(name, tpe)(options(name))
      else if (ma.isBoolean)    java.lang.Boolean.FALSE
      else if (ma.isOptional)   None
      else                      missing(name)
    }
    
    val mainMirror = {
      val im = currentMirror.reflect(this)
      im.reflectMethod(mainMethod)
    }
    val argArray = (mainArgs map determineValue).toArray
    mainMirror(argArray: _*)
  }
  
  
  def main(cmdline: Array[String]) {
    try {
      _opts = Options.parse(scala.collection.immutable.HashMap.newBuilder ++= mainArgs.map(x => (x.name, x)) result,
                            argInfos, cmdline: _*)
      callWithOptions()
    }
    catch {
      case UsageError(msg) =>
        println("Error: " + msg)
        println(usageMessage)
    }
  }
}
