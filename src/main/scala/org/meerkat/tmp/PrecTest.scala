package org.meerkat.tmp

import OperatorParsers._
import Parsers._
  
object Test1 {
  val E: Parsers.Nonterminal = nt("E") { E ~ "+" ~ E | "a" }
}

object Test2 {
  // explicit type
  val E: OperatorParsers.Nonterminal = op_nt("E") { E ~ "+" ~ E | "a" }
}

object Test3 {
  // the use of |> and no occurences of operator nonterminal on left or right ends 
  // to the left of |> is a static error 
  val E: OperatorParsers.Nonterminal = op_nt("E") { E ~ "+" ~ E |> "a" }
}

object Test4 {
  val open: Parsers.Terminal = "("
  val close: Parsers.Terminal = "("
   
  val E: OperatorParsers.Nonterminal = op_nt("E") { open ~ E ~ "+" ~ E ~ close ~ E |> "a" }
}

object Test5 {
  val open: Parsers.Terminal = "("
  val close: Parsers.Terminal = "("
   
  val E: OperatorParsers.Nonterminal 
    = op_nt("E") {(    open ~ E ~ close 
                    |  E ~ "+" ~ E 
                    |> "a" 
                 )}
}