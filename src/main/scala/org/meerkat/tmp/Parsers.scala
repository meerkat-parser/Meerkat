package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup
import scala.reflect.ClassTag
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.util.Visualization
import org.meerkat.sppf.Slot
import org.meerkat.tree.RuleType

object Parsers {
  
  import AbstractCPSParsers._
  
  implicit object obj1 extends CanBuildSequence[NonPackedNode, NonPackedNode] {
    
    type T = NonPackedNode
    type Sequence = Parsers.Sequence
    
    def sequence(p: AbstractSequence[NonPackedNode]): Sequence 
      = new Sequence { 
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def symbol = p.symbol
          def ruleType = p.ruleType
          def size = p.size
        }
    
    def index(a: T): Int = a.rightExtent
    def intermediate(a: T, b: T, p: Slot, sppfLookup: SPPFLookup): T = sppfLookup.getIntermediateNode(p, a, b)
    
    type SequenceBuilder = Parsers.SequenceBuilder
    def builderSeq(f: Slot => Sequence): SequenceBuilder = new SequenceBuilder { def apply(slot: Slot) = f(slot) }
  }
  
  implicit object obj2 extends CanBuildAlternation[NonPackedNode] {
    
    type Alternation = Parsers.Alternation
    def alternation(p: AbstractParser[NonPackedNode]): Alternation
      = new Alternation {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def symbol = p.symbol.asInstanceOf[org.meerkat.tree.Alt]
        }
    
    def result(e: NonPackedNode, p: Slot, nt: Head, sppfLookup: SPPFLookup): NonPackedNode = sppfLookup.getNonterminalNode(nt, p, e)
    
    type AlternationBuilder = Parsers.AlternationBuilder
    def builderAlt(f: Head => Alternation): AlternationBuilder = new AlternationBuilder { def apply(head: Head) = f(head) }
  }
  
  implicit object obj3 extends Memoizable[NonPackedNode] {
    type U = Int
    def value(t: NonPackedNode): Int = t.rightExtent
  }
  
  implicit object obj4 extends CanBuildNonterminal[NonPackedNode] {
    type Nonterminal = Parsers.Nonterminal
    def nonterminal(nt: String, p: AbstractParser[NonPackedNode]): Nonterminal 
      = new Nonterminal {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def symbol = org.meerkat.tree.Nonterminal(nt)
          def name = nt
          override def toString = name
        }
  }
  
  trait Sequence extends AbstractParser[NonPackedNode] with Slot { def size: Int; def symbol: org.meerkat.tree.Sequence }
  
  trait Alternation extends AbstractParser[NonPackedNode] { def symbol: org.meerkat.tree.Alt }
  
  trait Nonterminal extends Symbol { def symbol: org.meerkat.tree.Nonterminal }
  
  trait Terminal extends Symbol { def symbol: org.meerkat.tree.Terminal }
  
  trait SequenceBuilder extends (Slot => Sequence) { import AbstractParser._
    def ~ (p: Symbol): SequenceBuilder = seq(this, p)
    
    def | (p: AlternationBuilder): AlternationBuilder = altSeqAlt(this, p)
    def | (p: SequenceBuilder): AlternationBuilder = altSeq(this, p)
    def | (p: Symbol): AlternationBuilder = altSeqSym(this, p)
  }
  
  trait AlternationBuilder extends (Head => Alternation) { import AbstractParser._
    def | (p: AlternationBuilder): AlternationBuilder = altAlt(this, p)
    def | (p: SequenceBuilder): AlternationBuilder = altAltSeq(this, p)
    def | (p: Symbol): AlternationBuilder = altAltSym(this, p)
  }
  
  trait Symbol extends AbstractParser[NonPackedNode] { import AbstractParser._
    def name: String 
    
    def ~ (p: Symbol): SequenceBuilder = seq(this, p)
    
    def | (p: AlternationBuilder): AlternationBuilder = altSymAlt(this, p)
    def | (p: SequenceBuilder): AlternationBuilder = altSymSeq(this, p)
    def | (p: Symbol): AlternationBuilder = altSym(this, p)
  }
  
  def ntAlt(name: String, p: => AlternationBuilder): Nonterminal = nonterminalAlt(name, p)
  def ntSeq(name: String, p: => SequenceBuilder): Nonterminal = nonterminalSeq(name, p)
  def ntSym(name: String, p: Symbol): Nonterminal = nonterminalSym(name, p)
  
  implicit def toTerminal(s: String) 
    = new Terminal { 
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) 
          = if (input.startsWith(s, i)) { CPSResult.success(sppfLookup.getTerminalNode(s, i, i + s.length())) } 
            else CPSResult.failure
              
        def symbol = org.meerkat.tree.Terminal(s)
        def name = s
        override def toString = name
      }
  
  def run(input: Input, sppf: SPPFLookup, parser: Nonterminal): Unit = {
    parser(input, 0, sppf)(t => if(t.rightExtent == input.length) { println(s"Success: $t")  })
    Trampoline.run
  }
  
  def parse(sentence: String, parser: Nonterminal): Unit = {
    val input = new Input(sentence)
    val sppf = new DefaultSPPFLookup(input)
    
    run(input, sppf, parser)
    
    println(s"Trying to find: ${parser.name}(0,${sentence.length()})")
    val startSymbol = sppf.getStartNode(parser, 0, sentence.length())
    
    startSymbol match {
      case None       => println("Parse error")
      case Some(node) => println("Success: " + node)
                         println(sppf.countAmbiguousNodes + ", " + sppf.countIntermediateNodes + ", " + sppf.countPackedNodes + ", " + sppf.countNonterminalNodes + ", " + sppf.countTerminalNodes)
                         println("Visualizing...") 
                         Visualization.visualize(Visualization.toDot(startSymbol.get), "sppf")
                         println("Done!")
    }
  }
  
  object NonterminalBuilder {
    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox.Context
    
    def ^ (p: AlternationBuilder): Nonterminal = macro makeNonterminalAltWithName
    //implicit def seq(p: SequenceBuilder): Nonterminal = macro makeNonterminalSeqWithName
      
    def mkNtAlt(name: String, p: => AlternationBuilder): Nonterminal = ntAlt(name, p)
    // def mkNtSeq(name: String, p: => SequenceBuilder): Nonterminal = ntSeq(name, p)
    
    import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName

    def makeNonterminalAltWithName(c: Context)(p: c.Expr[AlternationBuilder]): c.Expr[Nonterminal] 
      = makeCallWithName (c, "NonterminalBuilder.mkNtAlt")
      
//    def makeNonterminalSeqWithName(c: Context)(p: c.Expr[SequenceBuilder]): c.Expr[Nonterminal] 
//      = makeCallWithName (c, "NonterminalBuilder.mkNtSeq")
  }
  
}