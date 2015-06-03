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
import org.meerkat.tree.Memoization

package object visualization {
  
  implicit val f: (SPPFNode, Input, Boolean) => String = toDot
  implicit val g: (Tree, Input, Boolean) => String = toDot

  def visualize[T](t: T, input: Input, name: String = "graph", path: String = ".", memoize: Boolean = true)(implicit f: (T, Input, Boolean) => String): Unit = visualize(f(t, input, memoize), name, path)
  
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
  
  def toDot(t: Tree, input: Input, memoize: Boolean): String = {
    // TODO: with memoization doesn't work: equals in nodes!!!
    val v = new TreeToDot
    v.visit(t)(input)
    v.get
  }
  
  def toDot(t: SPPFNode, input: Input, memoize: Boolean): String = {
    val v = if (memoize) new SPPFToDot with org.meerkat.sppf.Memoization else new SPPFToDot
    v.visit(t)
    v.get
  }


}