package org.meerkat.tree

import org.meerkat.util.Input
import scala.collection.mutable.HashMap
import org.meerkat.util.visualization.Shape._
import org.meerkat.util.visualization.Style._
import org.meerkat.util.visualization._

trait TreeVisitor {
  type T
  def visit(t: Tree)(implicit input: Input): T  
}

trait Memoization extends TreeVisitor {

  val cache = new HashMap[Tree, T]
  
  override abstract def visit(t: Tree)(implicit input: Input): T =
    cache.getOrElseUpdate(t, super.visit(t))
}


class TreeToDot extends TreeVisitor {
    type T = Unit
    
    val sb = new StringBuilder
    
    def get: String = sb.toString
    
    def visit(t: Tree)(implicit input: Input): T = t match {
      case n @ Epsilon()   => sb ++= getShape(n.id, "&epsilon;", Rectangle, Rounded)
    
      case n @ Terminal(s) => sb ++= getShape(n.id, "\"" + s + "\"", Rectangle, Rounded)
    
      case n @ Appl(r, s)  => {
        r match {
          case r: Rule        => sb ++= getShape(n.id, if (r.head.isRegular) s"${r.head}" else s"$r", Rectangle, Rounded)
          case r: PartialRule => sb ++= getShape(n.id, s"$r", Rectangle)
          case r: RegularRule => sb ++= getShape(n.id, s"$r", Rectangle)
        }
        s.foreach { t => addEdge(n.id, t.id, sb); visit(t) }      
      }
    
      case n @ Amb(s)      => {
        sb ++= getShape(n.id, "Amb", Diamond)
        s.foreach { t => addEdge(n.id, t.id, sb); visit(t) }
      } 
    }
}