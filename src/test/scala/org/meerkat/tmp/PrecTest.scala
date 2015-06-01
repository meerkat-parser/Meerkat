package org.meerkat.tmp

import OperatorParsers._
import Syntax._
  
object Test2 {
  val E: OperatorNonterminal = syn { E ~ "+" ~ E | "a" }
}

object Test3 {
  val E: OperatorNonterminal
  
    = syn (  "(" ~ E ~ ")" 
          |> right { E ~ "^" ~ E } 
          |> E ~ "+"
          |> left { E ~ "+" ~ E }
          |> "-" ~ E
          | "a" )
  
  def main(args: Array[String]): Unit = {
    // parse("((a))", E)
    parse("a+-a+a^a+^a+a", E)
  }
}

object Test4 {
  val E: OperatorNonterminal
  
    = syn (  "(" ~ E ~ ")" 
          |> right(  E ~ "*" ~ E 
                   | E ~ "/" ~ E )
          |> left(  E ~ "+" ~ E 
                  | E ~ "-" ~ E)
          |> "-" ~ E
          |  "a" )
  
  def main(args: Array[String]): Unit = {
    parse("a+a-a*a/a", E)
  }
}

object Test5 {
  val E: OperatorNonterminal 
    = syn (  "(" ~ E ~ ")" 
          |> right(  E ~ "*" ~ E 
                   | E ~ "/" ~ E )
          |  left(  E ~ "+" ~ E 
                  | E ~ "-" ~ E)
          |> "-" ~ E
          |  "a" )
  
  def main(args: Array[String]): Unit = {
    // parse("a+a-a*a/a", E) // (a+((a-a)*(a/a)))
    parse("a+a-a*a", E)
  }
}
