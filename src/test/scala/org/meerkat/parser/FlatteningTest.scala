package org.meerkat.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.meerkat.meerkat._
import org.meerkat.util.Visualization
import org.meerkat.sppf.SPPFVisitor
import org.meerkat.tree.Tree

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
    r.fold(a => println("Parse Error"), b => Visualization.toDot(b.sppf))
    val x = SPPFVisitor.buildTree(r.right.get.sppf)   
    println(x)
  }

  val S: MeerkatParser = "S" ::= S ~~ S ~~ S | S ~~ S | "b"
  
  def test2() {

    val r = new MeerkatParsers {} . parse(S, "bbb")
    r.fold(a => println("Parse Error"), b => Visualization.toDot(b.sppf))
    val x = SPPFVisitor.buildTree(r.right.get.sppf)   
    println(x)
  }
  
  def main(args: Array[String]): Unit = {
    test2()
  }
  
}