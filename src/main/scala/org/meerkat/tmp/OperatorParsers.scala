package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input

object OperatorParsers {
  
  import AbstractOperatorParsers._
  
  trait Sequence extends AbstractOperatorParser[NonPackedNode]
  
  case class Postfix(p1: AbstractOperatorParser[NonPackedNode], p2: AbstractCPSParsers.AbstractParser[NonPackedNode]) extends Sequence { import Parsers._
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1(prec), p2)
    
    def ~ (p: Parsers.Symbol): OperatorParsers.Sequence = Postfix(this, p)
    def ~ (p: OperatorParsers.Nonterminal): OperatorParsers.Sequence = Infix(this, p)
  }
  
  case class Prefix(p1: Parsers.Sequence, p2: Nonterminal) extends Sequence { import Parsers._
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1, p2(prec))
    
    def ~ (p: Parsers.Symbol): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(Prefix.this($), p)
    def ~ (p: OperatorParsers.Nonterminal): OperatorParsers.Sequence = Prefix(Prefix.this($), p)
  }
  
  case class Infix(p1: AbstractOperatorParser[NonPackedNode], p2: Nonterminal) extends Sequence { import Parsers._
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1(prec), p2(prec))
    
    def ~ (p: Parsers.Symbol): OperatorParsers.Sequence = Postfix(Postfix(p1, p2($)), p)
    def ~ (p: OperatorParsers.Nonterminal): OperatorParsers.Sequence = Infix(Postfix(p1, p2($)), p)
  }
  
  trait Nonterminal extends AbstractOperatorParser[NonPackedNode] {
    def ~ (p: Parsers.Symbol): Sequence = Postfix(this, p)
    def ~ (p: Nonterminal): Sequence = Infix(this, p)
  }
  

}