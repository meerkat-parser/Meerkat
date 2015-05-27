package org.meerkat.tmp

object Test {
  
  import Parsers._
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
  
  val A: Nonterminal = ntSym("A", "a")
  val B: Nonterminal = ntSeq("B", A.+ ~ "b")
  
  def main(args: Array[String]): Unit = {
    parse("aaab", B)
  }
}