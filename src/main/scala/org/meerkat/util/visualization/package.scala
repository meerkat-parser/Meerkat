/*
 * Copyright (c) 2015, Anastasia Izmaylova and Ali Afroozeh, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */

package org.meerkat.util

import org.meerkat.util.visualization.Shape._
import org.meerkat.util.visualization.Style._
import org.meerkat.util.visualization.Color._
import scala.sys.process._
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import org.meerkat.tree.Tree
import org.meerkat.sppf.NonPackedNode
import org.meerkat.tree.TreeVisitor
import org.meerkat.sppf.SPPFNode
import org.meerkat.tree.TreeToDot
import org.meerkat.sppf.SPPFToDot
import org.meerkat.sppf.Memoization

package object visualization {
  
  implicit val f: (SPPFNode, Input) => String = toDot
  implicit val g: (Tree, Input) => String = toDot

  def visualize[T](t: T, input: Input, name: String = "graph", path: String = ".")(implicit f: (T, Input) => String): Unit = visualize(f(t, input), name, path)
  
  private def visualize(s: String, name: String, path: String): Unit = {
        val sb = new StringBuilder
  
        sb ++= """| digraph sppf {
                  | layout=dot
                  | nodesep=.6
                  | ranksep=.4      
                  | ordering=out
                  |""".stripMargin
        
      sb ++= s
          
      sb ++= "}\n"
      
      val file = new File(s"$name.dot")  
      val writer = new BufferedWriter(new FileWriter(file))
      writer.write(sb.toString)
      writer.close()
      
      println(s"/usr/local/bin/dot -Tpdf -o $name.pdf $path/$name.dot")
      s"/usr/local/bin/dot -Tpdf -o $name.pdf $path/$name.dot"!
  }

  
  def getShape(id: Any, label: String, shape: Shape, style: Style = Default, color: Color = Black) = 
    s""""${escape(id)}"[$shape $style height=0.1, width=0.1, color=$color, fontcolor=$color, label="${escape(label)}", fontsize=10];\n""" 

  def addEdge(src: Any, dst: Any, sb: StringBuilder, color: Color = Black) {
    sb ++= s"""edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7]; "${escape(src)}" -> { "${escape(dst)}" }\n"""
  }
    
  def escape(s: Any): String = s.toString.replaceAll("\"", "\\\\\"").replaceAll("\t", "t").replaceAll("\n", "n").replaceAll("\r", "r")
  
  def toDot(t: Tree, input: Input): String = {
    val v = new TreeToDot
    v.visit(t)(input)
    v.get
  }
  
  def toDot(t: SPPFNode, input: Input): String = {
    val v = new SPPFToDot with Memoization
    v.visit(t)
    v.get
  }


}