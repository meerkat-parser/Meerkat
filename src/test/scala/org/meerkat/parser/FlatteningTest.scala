package org.meerkat.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.meerkat.meerkat._
import org.meerkat.util.Visualization
import org.meerkat.util.Visualization._
import org.meerkat.sppf.SPPFVisitor
import org.meerkat.tree.Tree
import org.meerkat.tree.Appl
import org.meerkat.tree.Amb
import org.meerkat.tree.Nonterminal
import org.meerkat.tree.Nonterminal

@RunWith(classOf[JUnitRunner])
class FlatteningTest extends FunSuite {
  
}

object FlatteningTest {
  
  def test1() {
    val A: MeerkatParser = "A" ::= "a"
    val B: MeerkatParser = "B" ::= "b"
    val C: MeerkatParser = "C" ::= "c"
    val D: MeerkatParser = "D" ::= "d"
    val E: MeerkatParser = "E" ::= "e"
    val S: MeerkatParser = "S" ::= A ~~ B ~~ C ~~ D ~~ E

    val r = new MeerkatParsers {} . parse(S, "abcde")
//    r.fold(a => println("Parse Error"), b => visualize(b.sppf))
    val x = SPPFVisitor.buildTree(r.right.get.sppf)   
    visualize(x)
  }

  val S: MeerkatParser = "S" ::= S ~~ S ~~ S | S ~~ S | "b"
  
  def test2() {

    val r = new MeerkatParsers {} . parse(S, "bbb")
    r.fold(a => println("Parse Error"), b => visualize(b.sppf))
    val x = SPPFVisitor.buildTree(r.right.get.sppf)
//    visualize(x)
    r.fold(a => println("Parse Error"), b => visualize(b.sppf))
    x match {
      case Appl(a, b) => println(b.length)
      case Amb(a)     => println(a.size)
    }
  }
  
  def main(args: Array[String]): Unit = {
    test2()
  }
  
}