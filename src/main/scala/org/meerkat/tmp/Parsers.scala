package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup
import scala.reflect.ClassTag
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.util.Visualization
import org.meerkat.sppf.Slot
import org.meerkat.tree.RuleType
import org.meerkat.sppf.SPPFVisitor
import org.meerkat.sppf.SemanticAction

object Parsers {
  
  import AbstractCPSParsers._
  import org.meerkat.tmp.Negation._
  
  sealed trait NoValue
  
  trait &[A,B] { type R }
  implicit def f1[A <: NoValue,B <: NoValue] = new &[NoValue,NoValue] { type R = NoValue }
  implicit def f2[A <: NoValue,B: ![NoValue]#f] = new &[NoValue,B] { type R = B }
  implicit def f3[A: ![NoValue]#f,B <: NoValue] = new &[A,NoValue] { type R = A }
  implicit def f4[A: ![NoValue]#f,B: ![NoValue]#f] = new &[A,B] { type R = (A,B) }
  
  implicit def obj1[ValA,ValB](implicit values: ValA & ValB) = new CanBuildSequence[NonPackedNode,NonPackedNode,ValA,ValB] {
    
    implicit val m1 = obj4; implicit val m2 = obj4
    
    type T = NonPackedNode; type V = values.R
      
    type Sequence = Parsers.Sequence
    def sequence(p: AbstractSequence[NonPackedNode]): Sequence 
      = new Sequence { 
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def size = p.size; def symbol = p.symbol; def ruleType = p.ruleType
          def update(f: Any => Any) = p update f
        }
      
      def index(a: T): Int = a.rightExtent
      def intermediate(a: T, b: T, p: Slot, sppfLookup: SPPFLookup): T = sppfLookup.getIntermediateNode(p, a, b)
      
      type SequenceBuilder = Parsers.SequenceBuilder { type Value = V }
      def builderSeq(f: Slot => Sequence) = new Parsers.SequenceBuilder { type Value = V; def apply(slot: Slot) = f(slot) }
  }
  
  implicit object obj2 extends CanBuildAlternative[NonPackedNode] {
    implicit val m = obj4
    def result(e: NonPackedNode, p: Slot, nt: Head, sppfLookup: SPPFLookup): NonPackedNode = sppfLookup.getNonterminalNode(nt, p, e)
  }
  
  implicit def obj3[ValA,ValB] = new CanBuildAlternation[NonPackedNode,NonPackedNode,ValA,ValB] {
    implicit val m1 = obj4; implicit val m2 = obj4
    implicit val o1 = obj2; implicit val o2 = obj2
    
    type Alternation = Parsers.Alternation
    def alternation(p: AbstractParser[NonPackedNode]): Alternation
      = new Alternation {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def symbol = p.symbol.asInstanceOf[org.meerkat.tree.Alt]
        }
    
    type AlternationBuilder = Parsers.AlternationBuilder { type Value = ValB }
    def builderAlt(f: Head => Alternation) = new Parsers.AlternationBuilder { type Value = ValB; def apply(head: Head) = f(head) }
  }
  
  implicit object obj4 extends Memoizable[NonPackedNode] {
    type U = Int
    def value(t: NonPackedNode): Int = t.rightExtent
  }
  
  implicit def obj5[Val] = new CanBuildNonterminal[NonPackedNode,Val] {
    implicit val m = obj4
    
    type Nonterminal = Parsers.Nonterminal { type Value = Val }
    def nonterminal(nt: String, p: AbstractParser[NonPackedNode]) 
      = new Parsers.Nonterminal {
          type Value = Val
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def symbol = org.meerkat.tree.Nonterminal(nt)
          def name = nt
          override def toString = name
        }
  }
  
  implicit def obj6[Val] = new CanBuildEBNF[NonPackedNode,Val] {
    implicit val m = obj4
    
    type T = NonPackedNode
    type Regular = Nonterminal { type Value = List[Val] }
    type Group = Nonterminal { type Value = Val }
    
    def regular(sym: org.meerkat.tree.Nonterminal, p: AbstractParser[NonPackedNode]): Regular 
      = new Nonterminal {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def symbol = sym
          def name = symbol.toString
          override def toString = name
          
          type Value = List[Val]
        }

    def group(symbol: org.meerkat.tree.Nonterminal, p: AbstractParser[NonPackedNode]): Group = ???
  }
  
  trait Sequence extends AbstractParser[NonPackedNode] with Slot { 
    def size: Int
    def symbol: org.meerkat.tree.Sequence
    def update(f: Any => Any): Unit
  }
  
  trait Alternation extends AbstractParser[NonPackedNode] { def symbol: org.meerkat.tree.Alt }
  
  type SemanticNonterminal[T] = Nonterminal { type Value = T }
  type SemanticTerminal[T] = Terminal { type Value = T }
  
  trait Nonterminal extends Symbol {
    def symbol: org.meerkat.tree.Nonterminal
    def input = obj5[String].nonterminal(this.name, this) 
  }
  
  trait Terminal extends Symbol {
    def symbol: org.meerkat.tree.Terminal
  }
  
  val epsilon = new Terminal { 
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = CPSResult.success(sppfLookup.getEpsilonNode(i))
    def symbol = org.meerkat.tree.Terminal(name)
    def name = "epsilon"
  }
  
  trait SequenceBuilder extends (Slot => Sequence) { import AbstractParser._
    
    type Value
    
    def ~ (p: Symbol)(implicit tuple: this.Value & p.Value) = { implicit val obj = obj1(tuple); seq(this, p) }
    
    def | (p: AlternationBuilder)(implicit sub: this.Value <:< p.Value) = altSeqAlt(this, p)
    def | (p: SequenceBuilder)(implicit sub: this.Value <:< p.Value) = altSeq(this, p)
    def | (p: Symbol)(implicit sub: this.Value <:< p.Value) = altSeqSym(this, p)
    
    def ^^[V](f: Value => V) = new SequenceBuilder { 
      def apply(slot: Slot) = { 
        val p = SequenceBuilder.this(slot)
        p update { x => f(x.asInstanceOf[SequenceBuilder.this.Value]) }; p 
      }
      type Value = V
    }
  }
  
  trait AlternationBuilder extends (Head => Alternation) { import AbstractParser._
    
    type Value
    
    def | (p: AlternationBuilder)(implicit sub: this.Value <:< p.Value) = altAlt(this, p)
    def | (p: SequenceBuilder)(implicit isSubtype: this.Value <:< p.Value) = altAltSeq(this, p)
    def | (p: Symbol)(implicit isSubtype: this.Value <:< p.Value) = altAltSym(this, p)
  }
  
  trait Symbol extends AbstractParser[NonPackedNode] { import AbstractParser._
    
    type Value  
    def name: String 
    
    def ~ (p: Symbol)(implicit tuple: this.Value & p.Value) = { implicit val obj = obj1(tuple); seq(this, p) }
    
    def | (p: AlternationBuilder)(implicit sub: this.Value <:< p.Value) = altSymAlt(this, p)
    def | (p: SequenceBuilder)(implicit sub: this.Value <:< p.Value) = altSymSeq(this, p)
    def | (p: Symbol)(implicit sub: this.Value <:< p.Value) = altSym(this, p)
    
    def epsilon[A] = new Terminal { 
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = CPSResult.success(sppfLookup.getEpsilonNode(i))
      def symbol = org.meerkat.tree.Terminal(name)
      def name = "epsilon"
      override def toString = name
      type Value = A
    }
//    var opt: Option[Nonterminal] = None
//    def ?(): Nonterminal = opt.getOrElse({ 
//      val p = regular(org.meerkat.tree.Opt(this.symbol), this | epsilon); opt = Option(p); p })
//      
    var star: Option[Nonterminal { type Value = List[Symbol.this.Value] }] = None
    def *(): Nonterminal = star.getOrElse({
      implicit val f4 = new &[List[Symbol.this.Value],Symbol.this.Value] { type R = List[Symbol.this.Value] }
      val p = regular[NonPackedNode,this.Value](org.meerkat.tree.Star(this.symbol), star.get ~ this | epsilon[List[this.Value]])
      star = Option(p); p })
//    
//    var plus: Option[Nonterminal] = None
//    def +(): Nonterminal = plus.getOrElse({
//      val p = regular(org.meerkat.tree.Plus(this.symbol), plus.get ~ this | this); plus = Option(p); p })
    
    def \(): Nonterminal = ???
    def !>>(): Nonterminal = ???
    def !<<(): Nonterminal = ???
  }
   
  def ntAlt[T](name: String, p: => AlternationBuilder { type Value = T }) = nonterminalAlt[NonPackedNode,T](name, p)  
  def ntSeq[T](name: String, p: => SequenceBuilder { type Value = T }) = nonterminalSeq[NonPackedNode,T](name, p)
  def ntSym(name: String, p: Symbol) = nonterminalSym(name, p)
  
  implicit def toTerminal(s: String) 
    = new Terminal { 
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) 
          = if (input.startsWith(s, i)) { CPSResult.success(sppfLookup.getTerminalNode(s, i, i + s.length())) } 
            else CPSResult.failure
              
        def symbol = org.meerkat.tree.Terminal(s)
        def name = s
        override def toString = name
        
        type Value = NoValue
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
                         import Visualization._
                         val x = SemanticAction.execute(node)(input)
                         println(s"WOW: $x")
                         // Visualization.visualize(Visualization.toDot(startSymbol.get), "sppf")
                         println("Done!")
    }
  }
  
  object NonterminalBuilder {
    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox.Context
    
    def ^ (p: AlternationBuilder): Nonterminal = macro makeNonterminalAltWithName
    //implicit def seq(p: SequenceBuilder): Nonterminal = macro makeNonterminalSeqWithName
      
    def mkNtAlt(name: String, p: => AlternationBuilder): Nonterminal = ??? // ntAlt(name, p)
    // def mkNtSeq(name: String, p: => SequenceBuilder): Nonterminal = ntSeq(name, p)
    
    import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName

    def makeNonterminalAltWithName(c: Context)(p: c.Expr[AlternationBuilder]): c.Expr[Nonterminal] 
      = makeCallWithName (c, "NonterminalBuilder.mkNtAlt")
      
//    def makeNonterminalSeqWithName(c: Context)(p: c.Expr[SequenceBuilder]): c.Expr[Nonterminal] 
//      = makeCallWithName (c, "NonterminalBuilder.mkNtSeq")
  }
  
}