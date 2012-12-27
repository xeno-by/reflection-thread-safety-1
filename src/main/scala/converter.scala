package optional

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

object Converter {
  def apply[T](conversions: Function1[T, Any]*)(implicit ct: TypeTag[T]) = {
    
  }
}

/**
 * Utility class that 
 */
class ConverterRegistry[I](implicit val itt: TypeTag[I]) {
  import scala.collection.mutable.{ HashMap => MHashMap }
  private val converters = new MHashMap[Type, I => Any]()
  
  def register[R](f: I => R)(implicit rtt: TypeTag[R]) {
    val rt: Type = rtt.tpe
    converters.put(rt, f)
  }
  def get[R](implicit rtt: TypeTag[R]): Option[I => R] = {
    val rt: Type = rtt.tpe
    converters.get(rt) match {
      case Some(cf) => Some(cf.asInstanceOf[I => R])
      case None => None
    }
  }
  def get(rt: Type): Option[I => Any] = converters.get(rt)
}