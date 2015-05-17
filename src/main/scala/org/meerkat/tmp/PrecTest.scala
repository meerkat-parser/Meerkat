package org.meerkat.tmp

import OperatorParsers._
import Parsers._
  
object Test1 {
  val E: Nonterminal = nt("E") { E ~ "+" ~ E | "a" }
}

object Test2 {
  val E: OperatorNonterminal = op_nt("E") { E ~ "+" ~ E | "a" }
}

object Test3 {
  val E: OperatorNonterminal 
    = op_nt("E") { "(" ~ E ~ ")" |> E ~ "*" ~ E |> right { E ~ "+" ~ E } | "a" }
  
  def main(args: Array[String]): Unit = {
    OperatorParsers.parse("a+a*a+a", E)  
  }
}