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
import org.meerkat.sppf.IntermediateNode
import org.meerkat.sppf.NonterminalNode
import org.meerkat.sppf.PackedNode
import org.meerkat.sppf.SPPFNode
import org.meerkat.sppf.TerminalNode
import org.meerkat.tree.Tree
import org.meerkat.sppf.NonPackedNode

object Visualization {
  
  implicit val f: NonPackedNode => String = toDot
  implicit val g: Tree => String = toDot

  def visualize[T](t: T, name: String = "sppf", path: String = ".")(implicit f: T => String): Unit = visualize(f(t), name, path)
  
  private def visualize(s: String, name: String, path: String): Unit = {
  		val sb = new StringBuilder
  
  		sb ++= """| digraph sppf {
  				      |     layout=dot
  				      |     nodesep=.6
  				      |     ranksep=.4		
  				      |     ordering=out
  		          |""".stripMargin
  		
  		sb ++= "}"
  		  
  		val file = new File("sppf.dot")  
  		val writer = new BufferedWriter(new FileWriter(file))
  		writer.write(sb.toString)
  		writer.close()
  		
  		s"/usr/local/bin/dot -Tpdf -o $name.pdf $path/$name.dot"!
  }
  
  def toDot(t: Tree): String = {
    ???
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
		    sb ++= s""""${escape(n.toString)}"[shape=box, style=rounded, height=0.1, width=0.1, color=black, fontcolor=black, label="($slot, $leftExtent, $rightExtent)", fontsize=10];\n"""
		    for(t <- n.children) toDot(t, sb, duplicateSet)
		    for(t <- n.children) addEdges(n, t, sb)
		    		    
		  case n@IntermediateNode(slot, leftExtent, rightExtent) => 
		    sb ++= s""""${escape(n.toString)}"[shape=box, height=0.2, width=0.4, color=black, fontcolor=black, label="(${escape(slot.toString)}, $leftExtent, $rightExtent)", fontsize=10];\n"""
		    for(t <- n.children) toDot(t, sb, duplicateSet)
		    for(t <- n.children) addEdges(n, t, sb)
		    		    
		  case n@TerminalNode(char, leftExtent, rightExtent) =>
		    sb ++= s""""${escape(n.toString)}"[shape=box, style=rounded, height=0.1, width=0.1, color=black, fontcolor=black, label="(${escape(char)}, $leftExtent, $rightExtent)", fontsize=10];\n"""
		    
		  case n@PackedNode(slot, parent) =>
//		  sb ++= s""""${escape(n.toString)}"[shape=diamond, height=0.1, width=0.1, color=black, fontcolor=black, label="(${escape(slot)}, $parent.pivot)", fontsize=10];\n"""
		    sb ++= s""""${escape(n.toString)}"[shape=diamond, height=0.1, width=0.1, color=black, fontcolor=black, label="", fontsize=10];\n"""
		    for(t <- n.children) {
		      toDot(t, sb, duplicateSet)
		      addEdges(n, t, sb)
		    }
		}
	}

	def escape(s: Any): String = s.toString.replaceAll("\"", "\\\\\"").replaceAll("\t", "t").replaceAll("\n", "n").replaceAll("\r", "r")
	
	def addEdges(src: SPPFNode, dst: SPPFNode, sb: StringBuilder): Unit = {
		sb ++= s"""edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7]; "${escape(src.toString)}" -> { "${escape(dst.toString)}" }\n"""
	}
  
}