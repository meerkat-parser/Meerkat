package org.meerkat.tmp

object Precedence {
  
  import org.meerkat.meerkat.Result
  import org.meerkat.meerkat.Result._

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
    
  }
  
  case class ~[T, F](_1: T, _2: F)
  
  def tryToUse(): Unit = { new ~(new ~(1, 2), 3) match { case x ~ y ~ z => x + y + z } }
  
  type MyMap = Int Map Int
  
  import Negation._
  
  trait DDParser[T] extends (Int => Result[T]) {
    def ~ [F : ![Unit]#f](p: DDParser[F]): DDParser[T ~ F] = ???
    def ~ (p: DDParser[Unit]): DDParser[T] = ???
    
    def | [F >: T](p: DDParser[F]): DDParser[F] = ???
    
    def ^^ [F](f: T => F): DDParser[F] = ???
  }
  
  trait DDParserUnit extends DDParser[Unit]
  
  val p1: DDParser[Int] = ???
  val p2: DDParser[Int] = ???
  val p3: DDParser[Int] = ???
  val p:  DDParser[Int ~ Int ~ Int] = (p1 ~ p2 ~ p3)
  val q = p ^^ { case x ~ _ ~ z => x + z }
  
  val p4: DDParser[Unit] = ???
  val r = (p1 ~ p4)
  
  
  def left     (p: OperatorParser): OperatorParser = ???
  def right    (p: OperatorParser): OperatorParser = ???
  def assoc    (p: OperatorParser): OperatorParser = ???
  def non_assoc(p: OperatorParser): OperatorParser = ???
  
  def m (p: OperatorParser): OperatorParser = ???
  
  implicit def operator(p: Parser): OperatorParser 
    = new OperatorParser { def apply(pr: Prec) = p }
  
  type NonterminalParser = Nothing
  type TerminalParser = Nothing
    
  val C: Parser = "c" // terminalOperatorParser
  val D: OperatorParser = C 
  lazy val A: OperatorParser = m { "a" ~ "b" ~ A | D(0) ~ C > left(D | C) }
                               
  
}