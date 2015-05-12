package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import java.util.HashMap

object OperatorParsers {
  
  import AbstractOperatorParsers._
  
  trait HasAlternationOp extends AbstractOperatorParser[NonPackedNode] {
    def | (p: Sequence): Alternation = {
      val res = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) }
      if (this.isAlternation) {
      }
      res
    }
    
    def | (p: Nonterminal): Alternation = {
      val res = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) }
      res
    }
    
    def | (p: Parsers.Sequence): Alternation = {
      val res = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p) }
      res
    }
    
    def | (p: Parsers.Symbol): Alternation = {
      val res = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p) }
      res
    }
    
    // TODO: propagate incrementing precedence level
    def |> (p: Sequence): Alternation = {     
      lazy val q: Alternation = alternation { import Parsers._;
        var l = -1
        lazy val p1 
          = if (this.isAlternation || this.isSequence) {
              l = q.head.precedence.counter; q.head.precedence.incr
              if (this isSequence) this assign q.head.precedence.counter 
              this headed q.head
              this
            } else this
        lazy val p2 = { p assign l; p headed q.head }
        prec => AbstractCPSParsers.AbstractParser.alt(p1(prec), p2(prec)) 
      }
      q
    }
    
    def |> (p: Nonterminal): Alternation = {
      alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) }
    }
    
  }
  
  trait Sequence extends HasAlternationOp {
    override def isSequence = true
  }
  
  trait Alternation extends HasAlternationOp {
    override def isAlternation = true
    
    private var recursives: Groups[NonPackedNode] = _
    override def merge1[U >: NonPackedNode](alt: AbstractOperatorParser[U]) 
      = (this.recursives._1 :+ alt, this.recursives._2)
      
    override def merge2[U >: NonPackedNode](alt: AbstractOperatorParser[U])
      = (this.recursives._1 :+ alt, this.recursives._2 :+ (this.recursives._1.length + 1))
      
    def reset(recursives: Groups[NonPackedNode]): Unit
      = this.recursives = recursives
  }
  
  def alternation(f: Prec => Parsers.Alternation): Alternation
    = new Alternation { 
        def apply(prec: Prec): Parsers.Alternation = f(prec)
      }
  
  trait Nonterminal extends HasAlternationOp {
    override def isNonterminal = true
    
    def ~ (p: Parsers.Symbol): Postfix = Postfix(this, p)
    def ~ (p: Nonterminal): Infix = Infix(this, p)
  }
  
  case class Postfix(p1: AbstractOperatorParser[NonPackedNode], p2: AbstractCPSParsers.AbstractParser[NonPackedNode]) extends Sequence { import Parsers._
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1(prec), p2)
    
    def ~ (p: Parsers.Symbol): Postfix = Postfix(this, p)
    def ~ (p: OperatorParsers.Nonterminal): Infix = Infix(this, p)
  }
  
  case class Prefix(p1: AbstractCPSParsers.AbstractParser[NonPackedNode], p2: Nonterminal) extends Sequence { import Parsers._
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1, p2(prec))
    
    def ~ (p: Parsers.Symbol): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(Prefix.this($), p)
    def ~ (p: OperatorParsers.Nonterminal): Prefix = Prefix(Prefix.this($), p)
  }
  
  case class Infix(p1: AbstractOperatorParser[NonPackedNode], p2: Nonterminal) extends Sequence { import Parsers._
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1(prec), p2(prec))
    
    def ~ (p: Parsers.Symbol): Postfix = Postfix(Postfix(p1, p2($)), p)
    def ~ (p: OperatorParsers.Nonterminal): Infix = Infix(Postfix(p1, p2($)), p)
  }
  
  def left(p: Infix): Infix = ???
  def right(p: Infix): Infix = ???
  
  def left(p: Alternation): Alternation = ???
  def right(p: Alternation): Alternation = ???
  
  implicit class ParsersOp(p: AbstractCPSParsers.AbstractParser[NonPackedNode]) {
    def ~ (q: OperatorParsers.Nonterminal): Prefix = Prefix(p, q)
    
    def | (q: Sequence): Alternation 
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(p, q(prec)) }
    
    def | (q: Nonterminal): Alternation
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(p, q(prec)) }
  }
  
  def op_nt(name: String)(p: => AbstractOperatorParser[NonPackedNode]): Nonterminal
    = new Nonterminal {
        import Parsers._
        val table: java.util.Map[Prec, Parsers.Nonterminal] = new HashMap()
        lazy val q: AbstractOperatorParser[NonPackedNode] = p headed this 
        
        def apply(prec: Prec) 
          = if (table.containsKey(prec)) table.get(prec) 
            else { 
              val nt = AbstractCPSParsers.memoize(q(prec), name + s"$prec")
              table.put(prec, nt)
              nt
            }
        
        override def precedence = new Precedence
        
      }

}