package org.meerkat.tmp

object Test {
  
  import Parsers._
  import org.meerkat.tmp.Negation._
//  import NonterminalBuilder.^
//  
//  val E: Nonterminal = ^ ( E ~ "*" ~ E 
//                         | E ~ "+" ~ E 
//                         | A )
//                        
//  val A: Nonterminal = ^ ("a" | "b")
//  
//  def main(args: Array[String]): Unit = {
//    parse("a*a+a", E)
//  }
  
//  val A: Nonterminal = ntSym("A", "a")
//  val B: Nonterminal = ntSeq("B", A.+ ~ "b")
//  
//  def main(args: Array[String]): Unit = {
//    parse("aaab", B)
//  }
  
  val A: SemanticNonterminal[String] = ntSym("A","a".input)
  val B: SemanticNonterminal[String] = ntSym("B","b".input)
  
  val S: SemanticNonterminal[String]  = ntAlt("S", A ~ B ^^ { case (s1,s2) => s"$s2 has been parsed after $s1" } | "b".input)
  
//  val E: SemanticNonterminal[(String,String)]  = ntAlt("E", A ~ B | "a".input ~ "b".input)
  
  def main(args: Array[String]): Unit = {
    parse("ab", S)
  }
  
//  trait LIST[T] {
//    def add(elem: T): LIST[T]
//  }
//  
//  trait Semantic[A, B] {
//    
//    type L = LIST[Value]
//    type Value
//  }
//  
//  
//  
//  trait NoValue
//  implicit def f[A <: NoValue, B <: NoValue] = new Semantic[A, B] { type Value = NoValue }
//  implicit def g[A, B] = new Semantic[A, B] { type Value = (A, B) }
//  
//  def fun[A, B](a: A, b: B)(implicit o: Semantic[A,B]): o.L = ???
//  
//  val a1: Int = ???
//  val a2: Int = ???
//  var v = fun(a1,a2)
//  v = v.add((a1,a2))
  
}