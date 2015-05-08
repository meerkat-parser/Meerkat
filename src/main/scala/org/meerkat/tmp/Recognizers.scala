package org.meerkat.tmp

import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup

object Recognizers {
  
  import AbstractCPSParsers._
  
  implicit object obj1 extends Composable[Int, Int] {
    type R = Int
    type Sequence = Recognizers.Sequence
    
    def sequence(f: (Input, Int, SPPFLookup) => Result[Int]): Sequence 
      = new Recognizers.Sequence { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) } 
    
    def index(a: Int): Int = a
    def intermediate(a: Int, b: Int, p: AbstractParser[Int], sppfLookup: SPPFLookup): Int = b
  }
  
  implicit object obj2 extends Alternative[Int, Int] {
    type Alternation = Recognizers.Alternation
    
    def alternation(f: (Input, Int, SPPFLookup) => Result[Int]): Alternation
      = new Alternation { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) }
    
    def result(e: Int, p: AbstractParser[Int], nt: AbstractParser[Any], sppfLookup: SPPFLookup): Int = e
  }
  
  implicit object obj3 extends Memoizable[Int] {
    type U = Int
    def value(t: Int) = t
  }
  
  implicit object obj4 extends CanBecomeNonterminal[Int] {
    type Nonterminal = Recognizers.Nonterminal
    def nonterminal(p: (Input, Int, SPPFLookup) => Result[Int]): Nonterminal 
      = new Nonterminal { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup) }
  }
  
  trait HasSequenceOp extends AbstractParser[Int] {
    def ~ (p: Symbol): Sequence = AbstractParser.seq(this, p)
  }
  
  trait HasAlternationOp extends AbstractParser[Int] {
    def | (p: Sequence): Alternation = AbstractParser.alt(this, p)
    def | (p: Symbol): Alternation = AbstractParser.alt(this, p)
  }
  
  trait Sequence extends HasSequenceOp with HasAlternationOp { 
    override def isSequence = true
  }
  
  trait Alternation extends HasAlternationOp { 
    override def isAlternation = true
  }
  
  trait Symbol extends HasSequenceOp with HasAlternationOp {
    override def isSymbol = true
  }
  
  trait Nonterminal extends Symbol { 
    override def isNonterminal = true 
  }
  
  def nt(name: String)(p: => AbstractParser[Int]): Nonterminal
    = memoize(p, name)
  
  trait Terminal extends Symbol {
    override def isTerminal = true
  }

}