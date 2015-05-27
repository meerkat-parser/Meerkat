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

    val input: Input = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    val r = new MeerkatParsers {} . parse(S, input)
    r.fold(a => println("Parse Error"), b => visualize(b.sppf))
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)
//    visualize(x)
//    r.fold(a => println("Parse Error"), b => visualize(b.sppf))
//    x match {
//      case Appl(a, b) => println(b.length)
//      case Amb(a)     => println(a.size)
//    }
  }
  
  def test3() {
    val A: MeerkatParser = "A" ::= "a"
    val S: MeerkatParser = "S" ::= A.**

    val input: Input = "aaaaa"
    val r = new MeerkatParsers {} . parse(S, input)
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)   
    visualize(x)
  }
  
  def test4() {
    val A: MeerkatParser = "A" ::= "a"
    val S: MeerkatParser = "S" ::= A.++

    val input: Input = "aaaaa"
    val r = new MeerkatParsers {} . parse(S, input)
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)   
    visualize(x)
  }
  
  def test5() {
    val A: MeerkatParser = "A" ::= "a"
    val B: MeerkatParser = "B" ::= "b"
    val S: MeerkatParser = "S" ::= "a" ~~ (A | B)
    
    val input: Input = "ab"
    val r = new MeerkatParsers {} . parse(S, input)
    
    visualize(r.right.get.sppf)
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)
    visualize(x)
  }

  
  def test6() {
    val A: MeerkatParser = "A" ::= "a"
    val B: MeerkatParser = "B" ::= "b"
    val S: MeerkatParser = "S" ::= A.** ~~ B.++

    val input: Input = "aaabbb"
    val r = new MeerkatParsers {} . parse(S, input)
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)   
    visualize(x)
  }
  
  def test7() {
    val A: MeerkatParser = "A" ::= "a"
    val S: MeerkatParser = "S" ::= A.** ~~ A.**

    val input: Input = "aaa"
    val r = new MeerkatParsers {} . parse(S, input)
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)   
    visualize(x)
  }
  
    val E: MeerkatParser = "E" ::= ( E ~~ "+" ~~ E
                                 | E ~~ "*" ~~ E
                                 | "a"
                                 )
 
  def test8() {
    val input: Input = "a+a*a+a"
    val r = new MeerkatParsers {} . parse(E, input)
    val x = SPPFVisitor.buildTree(r.right.get.sppf)(input)   
    visualize(x)
  }
  
  
  def main(args: Array[String]): Unit = {
    test8()
  }
  
}