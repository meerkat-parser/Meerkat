package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup
import scala.reflect.ClassTag
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.util.Visualization

object Parsers {
  
  import AbstractCPSParsers._
  
  implicit object obj1 extends CanBuildSequence[NonPackedNode, NonPackedNode] {
    type R = NonPackedNode
    
    type Sequence = Parsers.Sequence
    
    def sequence(f: (Input, Int, SPPFLookup) => Result[NonPackedNode]): Sequence 
      = new Sequence { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = f(input, i, sppfLookup) } 
    
    def index(a: NonPackedNode): Int = a.rightExtent
    def intermediate(a: NonPackedNode, b: NonPackedNode, p: AbstractParser[NonPackedNode], sppfLookup: SPPFLookup): NonPackedNode 
      = sppfLookup.getIntermediateNode(p, a, b) 
  }
  
  implicit object obj2 extends CanBuildAlternation[NonPackedNode] {
    type Alternation = Parsers.Alternation
    
    def alternation(f: AbstractParser[Any] => (Input, Int, SPPFLookup) => Result[NonPackedNode]): Alternation
      = new Alternation { 
          lazy val p: (Input, Int, SPPFLookup) => Result[NonPackedNode] = f(head.getOrElse(this))
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
        }
    
    def result(e: NonPackedNode, p: AbstractParser[NonPackedNode], nt: AbstractParser[Any], sppfLookup: SPPFLookup): NonPackedNode
      = sppfLookup.getNonterminalNode(nt, p, e)
  }
  
  implicit object obj3 extends Memoizable[NonPackedNode] {
    type U = Int
    def value(t: NonPackedNode): Int = t.rightExtent
  }
  
  implicit object obj4 extends CanBuildNonterminal[NonPackedNode] {
    type Nonterminal = Parsers.Nonterminal
    
    def nonterminal(nt: String, f: AbstractParser[Any] => (Input, Int, SPPFLookup) => Result[NonPackedNode]): Nonterminal 
      = new Nonterminal {
          lazy val p: (Input, Int, SPPFLookup) => Result[NonPackedNode] = f(this)
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          
          override def name = nt
        }
  }
  
  trait HasSequenceOp extends AbstractParser[NonPackedNode] {
    def ~ (p: Symbol): Sequence = AbstractParser.seq(this, p)
  }
  
  trait HasAlternationOp extends AbstractParser[NonPackedNode] {
    def | (p: Sequence): Alternation = AbstractParser.alt(this, p)
    def | (p: Symbol): Alternation = AbstractParser.alt(this, p)
  }
  
  trait Sequence extends HasSequenceOp with HasAlternationOp { 
    override def isSequence = true
    private val nm = s"p${this.hashCode()}"
    override def name = nm
  }
  
  trait Alternation extends HasAlternationOp { 
    override def isAlternation = true
    
    private val nm = s"p${this.hashCode()}"
    override def name = nm
    
    private var hd: Option[AbstractParser[Any]] = None
    override def head: Option[AbstractParser[Any]] = hd
    override def pass(head: AbstractParser[Any]) = hd = Option(head)
  }
  
  trait Symbol extends HasSequenceOp with HasAlternationOp {
    override def isSymbol = true
  }
  
  trait Nonterminal extends Symbol { 
    override def isNonterminal = true 
  }
  
  trait Terminal extends Symbol { 
    override def isTerminal = true 
  }
  
  def nt(name: String)(p: => AbstractParser[NonPackedNode]): Nonterminal
    = memoize(p, name)
    
  implicit def terminal(s: String): Terminal 
    = new Terminal { 
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = {
          if (input.startsWith(s, i)) {
            CPSResult.success(sppfLookup.getTerminalNode(s, i, i + s.length()))
          } else CPSResult.failure
        }
        
        override def name = s
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
  
  object SyntaxBuilder {
    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox.Context
    
    implicit def toSyntax(p: => AbstractParser[NonPackedNode]): Unit => AbstractParser[NonPackedNode]
      = { _ => p }
    
    def ::(p: Unit => AbstractParser[NonPackedNode]): Nonterminal = macro makeNonterminalWithName
      
    def mkSyntax(name: String, p: Unit => AbstractParser[NonPackedNode]): Nonterminal = nt(name)(p())
    
    import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName

    def makeNonterminalWithName(c: Context)(p: c.Expr[Unit => AbstractParser[NonPackedNode]]): c.Expr[Nonterminal] 
      = makeCallWithName (c, "SyntaxBuilder.mkSyntax")
  }
  
}