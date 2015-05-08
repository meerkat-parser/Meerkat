package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input

object OperatorParsers {
  
  import AbstractOperatorParsers._
  
  trait Sequence extends AbstractOperatorParser[NonPackedNode]
  
  case class Postfix(p1: AbstractOperatorParser[NonPackedNode], p2: Parsers.Symbol) extends Sequence {
    def apply(prec: Prec) = ???
    
    def ~ (p: Parsers.Symbol): Sequence = Postfix(this, p)
    def ~ (p: Nonterminal): Sequence = Infix(this, p)
  }
  
  case class Prefix(p1: Sequence, p2: Nonterminal) extends Sequence {
    def apply(prec: Prec) = ???
    
    def ~ (p: Parsers.Symbol): Parsers.Sequence 
      = new Parsers.Sequence { import Parsers._
         val f = AbstractCPSParsers.AbstractParser.seq(Prefix.this($), p)
         def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup)
        }
    def ~ (p: Nonterminal): Sequence = ???
  }
  
  case class Infix(p1: Postfix, p2: Nonterminal) extends Sequence {
    def apply(prec: Prec) = ???
    
    def ~ (p: Parsers.Symbol): Sequence = ???
    def ~ (p: Nonterminal): Sequence = ???
  }
  
  trait Nonterminal extends AbstractOperatorParser[NonPackedNode] {
    def ~ (p: Parsers.Symbol): Sequence = ???
    def ~ (p: Nonterminal): Sequence = ???
  }
  

}