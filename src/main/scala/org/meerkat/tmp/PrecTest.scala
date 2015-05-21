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
    = op_nt("E")(   "(" ~ E ~ ")" 
                 |> right { E ~ "^" ~ E } 
                 |> E ~ "+"
                 |> left { E ~ "+" ~ E }
                 |> "-" ~ E
                 | "a" )
  
  def main(args: Array[String]): Unit = {
    // OperatorParsers.parse("((a))", E)
    OperatorParsers.parse("a+-a+a^a+^a+a", E)
  }
}

object Test4 {
  val E: OperatorNonterminal 
    = op_nt("E")(  "(" ~ E ~ ")" 
                 |> left(  E ~ "*" ~ E 
                         | E ~ "/" ~ E )
                 |> left(  E ~ "+" ~ E 
                         | E ~ "-" ~ E)
                 |>  "-" ~ E
                 |   "a" )
  
  def main(args: Array[String]): Unit = {
    OperatorParsers.parse("a+a-a*a/a", E)
  }
}