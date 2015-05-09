package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input

object OperatorParsers {
  
  import AbstractOperatorParsers._
  
  trait HasAlternationOp extends AbstractOperatorParser[NonPackedNode] {
    def | (p: Sequence): Alternation 
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) }
    
    def | (p: Nonterminal): Alternation
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) }
    
    def | (p: Parsers.Sequence): Alternation
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p) }
    
    def | (p: Parsers.Symbol): Alternation
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p) }
    
    // TODO: propagate incrementing precedence level
    def |> (p: Sequence): Alternation 
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) }
    
    def |> (p: Nonterminal): Alternation
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) }
    
  }
  
  trait Sequence extends HasAlternationOp
  
  trait Alternation extends HasAlternationOp
  
  def alternation(f: Prec => Parsers.Alternation): Alternation
    = new Alternation { 
          def apply(prec: Prec): Parsers.Alternation = f(prec)
        }
  
  trait Nonterminal extends HasAlternationOp {
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
  
  implicit class ParsersOp(p: AbstractCPSParsers.AbstractParser[NonPackedNode]) {
    def ~ (q: OperatorParsers.Nonterminal): Prefix = Prefix(p, q)
    
    def | (q: Sequence): Alternation 
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(p, q(prec)) }
    
    def | (q: Nonterminal): Alternation
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(p, q(prec)) }
  }
  
  def op_nt(name: String)(p: => AbstractOperatorParser[NonPackedNode]): Nonterminal
    = ???

}