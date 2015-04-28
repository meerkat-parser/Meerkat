package org.meerkat.tmp

object Precedence {
  
  import meerkat.Result
  import meerkat.Result._

  // Recognizer
  trait Parser extends (Int => Result[Int]) {
    def ~ (p: Parser): Parser = ???
    def | (p: Parser): Parser = ???
  }
  
  implicit def terminalParser(s: String): Parser
    = new Parser { def apply(i: Int) = success(i) }
  
  def left (p: Parser): Parser = ???
  def right(p: Parser): Parser = ???
  
  // Precedence level
  type Prec = Int
  
  trait OperatorParser extends (Prec => Int => Result[Int]) {
    def ~ (p: OperatorParser): OperatorParser = ???
    def | (p: OperatorParser): OperatorParser = ???
    def > (p: OperatorParser): OperatorParser = ???
  }
  
  def left (p: OperatorParser): OperatorParser = ???
  
  implicit def operator(p: Parser): OperatorParser 
    = new OperatorParser { def apply(pr: Prec) = p }
  
  
  val C: Parser = "c" // terminalOperatorParser
  val D: OperatorParser = C 
  lazy val A: OperatorParser = ( "a" ~ "b" ~ A 
                                  > D ~ C 
                                  > left(D | C) ) 
                               
  
}