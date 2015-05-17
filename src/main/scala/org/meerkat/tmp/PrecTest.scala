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
  // the use of |> and no occurences of operator nonterminal on left or right ends 
  // to the left of |> is a static error 
  val E: OperatorNonterminal 
    = op_nt("E") { E ~ "*" ~ E |> right { E ~ "+" ~ E } | "a" }
  
  def main(args: Array[String]): Unit = {
    OperatorParsers.parse("a+a*a+a", E)  
  }
}

object Test4 {
  val open: Parsers.Terminal = "("
  val close: Parsers.Terminal = ")"
   
  val E: Parsers.Nonterminal = nt("E") { open ~ E ~ "+" ~ E ~ close | "a" }
}

object Test5 {
  val open: Terminal = "("
  val close: Terminal = ")"
   
  val E: OperatorNonterminal 
    = op_nt("E") {(    open ~ E ~ close 
                    |  E ~ "*" ~ E
                    |> E ~ "+" ~ E
                    |  "a" 
                 )}
}

//object Test6 {
//  val open: Parsers.Terminal = "("
//  val close: Parsers.Terminal = ")"
//  
//  val E: OperatorParsers.Nonterminal 
//    = op_nt("E") {(    open ~ E ~ close 
//                    |  left { E ~ "*" ~ E }
//                    |> E ~ "+" ~ E
//                    |  "a" 
//                 )}
//}