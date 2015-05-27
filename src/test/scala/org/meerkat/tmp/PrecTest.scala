package org.meerkat.tmp

import OperatorParsers._
import Parsers._
  
object Test1 {
  val E: Nonterminal = Parsers.ntAlt("E", { E ~ "+" ~ E | "a" })
}

object Test2 {
  val E: OperatorNonterminal = OperatorParsers.ntAlt("E", { E ~ "+" ~ E | "a" })
}

object Test3 {
  val E: OperatorNonterminal
  
    = OperatorParsers.ntAlt("E", 
        (  "(" ~ E ~ ")" 
        |> right { E ~ "^" ~ E } 
        |> E ~ "+"
        |> left { E ~ "+" ~ E }
        |> "-" ~ E
        | "a" ))
  
  def main(args: Array[String]): Unit = {
    // OperatorParsers.parse("((a))", E)
    OperatorParsers.parse("a+-a+a^a+^a+a", E)
  }
}

object Test4 {
  val E: OperatorNonterminal 
    = OperatorParsers.ntAlt("E",
        (  "(" ~ E ~ ")" 
        |> right(  E ~ "*" ~ E 
                 | E ~ "/" ~ E )
        |> left(  E ~ "+" ~ E 
                | E ~ "-" ~ E)
        |> "-" ~ E
        |  "a" ))
  
  def main(args: Array[String]): Unit = {
    OperatorParsers.parse("a+a-a*a/a", E)
  }
}

object Test5 {
  val E: OperatorNonterminal 
    = OperatorParsers.ntAlt("E",
        (  "(" ~ E ~ ")" 
        |> right(  E ~ "*" ~ E 
                 | E ~ "/" ~ E )
        |  left(  E ~ "+" ~ E 
                | E ~ "-" ~ E)
        |> "-" ~ E
        |  "a" ))
  
  def main(args: Array[String]): Unit = {
    // OperatorParsers.parse("a+a-a*a/a", E) // (a+((a-a)*(a/a)))
    OperatorParsers.parse("a+a-a*a", E)
  }
}

object Test6 {
  val E: Nonterminal 
    = Parsers.ntAlt("E",
        ( "(" ~ E ~ ")" 
        | E ~ "*" ~ E 
        | E ~ "/" ~ E
        | E ~ "+" ~ E 
        | E ~ "-" ~ E
        |  "-" ~ E
        |   "a" ))
  
  def main(args: Array[String]): Unit = {
    Parsers.parse("a+a-a*a", E) // E(0,7) E(0,5) E(2,7)
  }
  
/*
E(2,2)(2, 7)
E(2,2)(0, 7)
E(2,2)(0, 5)
E(3,2)(0, 5)
E(3,2)(0, 7)
E(0,0)(0, 7)
E(0,2)(2, 7)
E(0,0)(0, 5)
*/
}