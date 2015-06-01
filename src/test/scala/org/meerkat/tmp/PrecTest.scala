package org.meerkat.tmp

import OperatorParsers._
import Syntax._
  
object Test1 {
  val E: OperatorNonterminal = syn { E ~ "+" ~ E | "a" }
}

object Test2 {
  
  val toStr: String => String = x => x
  
  val E: OperatorNonterminal & String 
    = syn ( E ~ "+" ~ E & { case (x,y) => s"($x) + ($y)" } | "9" ^ toStr )
    
  def main(args: Array[String]): Unit = {
    parse("9+9", E)
  }
}

object Test3 {
  
  val toInt: String => Int = x => x.toInt
  
  val E: OperatorNonterminal & Int
  
    = syn (  "(" ~ E ~ ")"
          |> right { E ~ "*" ~ E } & { case (x,y) => x*y } 
          |> E ~ "+"
          |> left { E ~ "+" ~ E }  & { case (x,y) => x+y }
          |> "-" ~ E               & {x => -x }
          | "3"                    ^ toInt )
  
  def main(args: Array[String]): Unit = {
    // parse("((a))", E)
    parse("3+-3+3*3+*3+3", E) // 3+(-(3+(((3*3)+)*3)+3)) == -30
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
