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
  
  // Precedence level
  type Prec = Int
  
  trait OperatorParser extends (Prec => Parser) {
    
    private var level: Int = 0
    
    private def setLevel(level: => Int): Unit = this.level = level 
    
    def ~ (p: OperatorParser): OperatorParser = { 
      ??? 
    }
    
    def | (p: OperatorParser): OperatorParser = { 
      ??? 
    }
    
    def > (p: OperatorParser): OperatorParser = {
      val q = this | p
      
      val r = this preFilter { pr => q.level + 1 >= pr } // different predicates
      
      this setLevel (q.level + 1)
      p setLevel q.level
      q
    }
    
    def preFilter(pred: Prec => Boolean): OperatorParser = ???
    def preFilter(pred: (Prec, Int) => Boolean): OperatorParser = ???
    
    def O(): Parser = ???
    
  }
  
  def left     (p: OperatorParser): OperatorParser = ???
  def right    (p: OperatorParser): OperatorParser = ???
  def assoc    (p: OperatorParser): OperatorParser = ???
  def non_assoc(p: OperatorParser): OperatorParser = ???
  
  implicit def operator(p: Parser): OperatorParser 
    = new OperatorParser { def apply(pr: Prec) = p }
    
  val C: Parser = "c" // terminalOperatorParser
  val D: OperatorParser = C 
  lazy val A: OperatorParser = ( "a" ~ "b" ~ A | D(0) ~ C > left(D | C) )
                               
  
}