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
import org.meerkat.util.Input
import org.meerkat.util.Input._


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

    val input: Input = "abcde"
    val r = new MeerkatParsers {} . parse(S, input)
//    r.fold(a => println("Parse Error"), b => visualize(b.sppf))
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)
    visualize(x)
//    println(SPPFVisitor.concat(r.right.get.sppf)(input))    
  }

  val S: MeerkatParser = "S" ::= S ~~ S ~~ S | S ~~ S | "b"
  
  def test2() {

    val input: Input = "bbb"
    val r = new MeerkatParsers {} . parse(S, input)
    r.fold(a => println("Parse Error"), b => visualize(b.sppf))
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)
    visualize(x)
//    r.fold(a => println("Parse Error"), b => visualize(b.sppf))
    x match {
      case Appl(a, b) => println(b.length)
      case Amb(a)     => println(a.size)
    }
  }
  
  def test3() {
    val A: MeerkatParser = "A" ::= "a"
    val S: MeerkatParser = "S" ::= A.**

    val input: Input = "aaaa"
    val r = new MeerkatParsers {} . parse(S, input)
    visualize(r.right.get.sppf)
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)   
//    visualize(x)
  }
  
  def test4() {
    val A: MeerkatParser = "A" ::= "a"
    val B: MeerkatParser = "B" ::= "b"
    val S: MeerkatParser = "S" ::= "a" ~~ (A | B)
    
    val input: Input = "ab"
    val r = new MeerkatParsers {} . parse(S, input)
    
    visualize(r.right.get.sppf)
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)
    visualize(x)
  }

  
  def main(args: Array[String]): Unit = {
    test1()
  }
  
}