package org.meerkat.tmp

import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup

object Recognizers extends AbstractParsers {
  
  type Result[+T] = CPSResult[T]
  
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
  
  trait Sequence extends AbstractParser[Int] { 
    override def isSequence = true
    
    def ~ (p: Symbol): Sequence = AbstractParser.seq(this, p)
  }
  
  trait Alternation extends AbstractParser[Int] { 
    override def isAlternation = true
    
    def | (p: Sequence): Alternation = AbstractParser.alt(this, p)
    def | (p: Symbol): Alternation = AbstractParser.alt(this, p)
  }
  
  trait Symbol extends AbstractParser[Int] {
    def ~ (p: Symbol): Sequence = AbstractParser.seq(this, p)
    def | (p: Symbol): Alternation = AbstractParser.alt(this, p)
  }
  
  trait Nonterminal extends Symbol { 
    override def isNonterminal = true 
  }
  
  trait Terminal extends Symbol {
    override def isTerminal = true
  }

}