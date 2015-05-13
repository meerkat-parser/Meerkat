package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import java.util.HashMap

object OperatorParsers {
  
  import AbstractOperatorParsers._
  
  trait HasAlternationOp extends AbstractOperatorParser[NonPackedNode] {
    def | (p: Sequence): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) })
        .set(this or p)
    
    def | (p: Nonterminal): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) })
        .set(this or p)
    

    def | (p: Parsers.Sequence): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p) })
        .set(this.asGroups)
    
    def | (p: Parsers.Symbol): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p) })
        .set(this.asGroups)
    
    def |> (p: Sequence): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) })
        .set(this greater p)
    
    def |> (p: Nonterminal): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) })
        .set(this greater p)
    
  }
  
  trait Sequence extends HasAlternationOp {
    override def isSequence = true
  }
  
  trait Alternation extends HasAlternationOp {
    override def isAlternation = true
    
    private var recursives: Groups[NonPackedNode] = _
    
    override def or[U >: NonPackedNode](alt: AbstractOperatorParser[U]) 
      = (this.recursives._1 :+ alt, this.recursives._2)
      
    override def greater[U >: NonPackedNode](alt: AbstractOperatorParser[U])
      = (this.recursives._1 :+ alt, this.recursives._2 :+ (this.recursives._1.length + 1))
      
    override def asGroups: Groups[NonPackedNode] = recursives
      
    def set(recursives: Groups[NonPackedNode]): Alternation
      = { this.recursives = recursives; this }
    
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