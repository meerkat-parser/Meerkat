/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package org.meerkat.util

import scala.collection.mutable._
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.sys.process._
import org.meerkat.sppf._
import org.meerkat.tree.Tree
import org.meerkat.tree._

object Visualization {
  
  import Shape._
  import Style._
  
  implicit val f: NonPackedNode => String = toDot
  implicit val g: Tree => String = toDot

  def visualize[T](t: T, name: String = "graph", path: String = ".")(implicit f: T => String): Unit = visualize(f(t), name, path)
  
  private def visualize(s: String, name: String, path: String): Unit = {
  		val sb = new StringBuilder
  
  		sb ++= """| digraph sppf {
  				      |     layout=dot
  				      |     nodesep=.6
  				      |     ranksep=.4		
  				      |     ordering=out
  		          |""".stripMargin
  		
      sb ++= s
  		  
      sb ++= "}\n"
      
  		val file = new File(s"$name.dot")  
  		val writer = new BufferedWriter(new FileWriter(file))
  		writer.write(sb.toString)
  		writer.close()
  		
  		s"/usr/local/bin/dot -Tpdf -o $name.pdf $path/$name.dot"!
  }
  
  def toDot(t: Tree): String = {
    val sb = new StringBuilder
    toDot(t, sb)
    sb.toString
  }
  
  private def toDot(t: Tree, sb: StringBuilder): Unit = t match {
    
    case n @ Terminal(s) => sb ++= getShape(n.id, "\"" + s + "\"", Rectangle, Rounded)
    
    case n @ Appl(r, s)  => {
      sb ++= getShape(n.id, r.toString, Rectangle, Rounded)
      s.foreach { t => addEdge(n.id, t.id, sb); toDot(t, sb) }
    }
    
    case n @ Amb(s)      => {
      sb ++= getShape(n.id, "Amb", Diamond)
      s.foreach { t => addEdge(n.id, t.id, sb); toDot(t, sb) }
    }
  }
    
  def toDot(node: NonPackedNode): String = {
   val sb = new StringBuilder
   toDot(node, sb, new HashSet())
   sb.toString
  }
  
	private def toDot(node: SPPFNode, sb: StringBuilder, duplicateSet: Set[SPPFNode]) : Unit = {
	  
	  if (!duplicateSet.contains(node)) {
	    duplicateSet.add(node)
	  } else {
	    return
	  }
	  
		node match {
		  case n@NonterminalNode(slot, leftExtent, rightExtent) => 
		    sb ++= getShape(n.toString(), s"($slot, $leftExtent, $rightExtent)", Rectangle, Rounded)
		    for(t <- n.children) toDot(t, sb, duplicateSet)
		    for(t <- n.children) addEdge(n.toString, t.toString, sb)
		    		    
		  case n@IntermediateNode(slot, leftExtent, rightExtent) =>
        sb ++= getShape(n.toString(), s"$slot, $leftExtent, $rightExtent", Rectangle)
		    for(t <- n.children) toDot(t, sb, duplicateSet)
		    for(t <- n.children) addEdge(n.toString, t.toString, sb)
		    		    
		  case n@TerminalNode(char, leftExtent, rightExtent) =>
        sb ++= getShape(n.toString, char.toString, Rectangle, Rounded)
		    sb ++= s""""${escape(n.toString)}"[shape=box, style=rounded, height=0.1, width=0.1, color=black, fontcolor=black, label="(${escape(char)}, $leftExtent, $rightExtent)", fontsize=10];\n"""
		    
		  case n@PackedNode(slot, parent) =>
//      sb ++= getShape(n.toString, s"($slot, ${n.pivot})", Diamond)
        sb ++= getShape(n.toString, "", Diamond)
		    for(t <- n.children) {
		      toDot(t, sb, duplicateSet)
		      addEdge(n.toString, t, sb)
		    }
		}
	}

	def escape(s: Any): String = s.toString.replaceAll("\"", "\\\\\"").replaceAll("\t", "t").replaceAll("\n", "n").replaceAll("\r", "r")
	
  
  def addEdge(src: Any, dst: Any, sb: StringBuilder) {
    sb ++= s"""edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7]; "${escape(src)}" -> { "${escape(dst)}" }\n"""
  }
  
  def getShape(id: Any, label: String, shape: Shape, style: Style = Default) = 
    s""""${escape(id)}"[$shape $style height=0.1, width=0.1, color=black, fontcolor=black, label="${escape(label)}", fontsize=10];\n""" 
  
}

object Shape extends Enumeration {
  
  type Shape = Value
  
  val Rectangle = Shape("shape = box, ")
  val Diamond   = Shape("shape = diamond, ")
  val Circle    = Shape("shape = circle,")

  def apply(s: String): Shape = Value(s)
}

object Style extends Enumeration {
  type Style = Value
  
  val Default = Style("")
  val Rounded = Style("style = rounded, ")
  
  def apply(s: String): Style = Value(s)
}

