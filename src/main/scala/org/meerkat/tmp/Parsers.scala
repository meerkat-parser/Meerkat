package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup
import scala.reflect.ClassTag
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.util.visualization._
import org.meerkat.sppf.Slot
import org.meerkat.tree.RuleType
import org.meerkat.sppf.SPPFVisitor
import org.meerkat.sppf.SemanticAction
import org.meerkat.sppf.TreeBuilder

object Parsers { import AbstractCPSParsers._; import org.meerkat.tmp.Negation._
  
  implicit def obj1[ValA,ValB](implicit vals: ValA & ValB) = new CanBuildSequence[NonPackedNode,NonPackedNode,ValA,ValB] {
    implicit val m1 = obj4; implicit val m2 = obj4
    
    type T = NonPackedNode; type V = vals.R
      
    type Sequence = Parsers.Sequence
    def sequence(p: AbstractSequence[NonPackedNode]): Sequence 
      = new Sequence { 
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input,i,sppfLookup)
          def size = p.size; def symbol = p.symbol; def ruleType = p.ruleType
        }  
    def index(a: T): Int = a.rightExtent
    def intermediate(a: T, b: T, p: Slot, sppfLookup: SPPFLookup): T = sppfLookup.getIntermediateNode(p, a, b)
      
    type SequenceBuilder = Parsers.SequenceBuilder { type Value = V }
    def builderSeq(f: Slot => Sequence) = new Parsers.SequenceBuilder { type Value = V; def apply(slot: Slot) = f(slot) }
  }
  
  implicit object obj2 extends CanBuildAlternative[NonPackedNode] {
    implicit val m = obj4
    def result(e: NonPackedNode, p: Slot, nt: Head, sppfLookup: SPPFLookup) = sppfLookup.getNonterminalNode(nt, p, e)
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
    
    type Nonterminal = Parsers.AbstractNonterminal { type Value = Val }
    def nonterminal(nt: String, p: AbstractParser[NonPackedNode]) 
      = new Parsers.AbstractNonterminal {
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
    type Regular = AbstractNonterminal { type Value = List[Val] }
    type Group = AbstractNonterminal { type Value = Val }
    
    def regular(sym: org.meerkat.tree.Nonterminal, p: AbstractParser[NonPackedNode]): Regular 
      = new AbstractNonterminal {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def symbol = sym
          def name = symbol.toString
          override def toString = name   
          type Value = List[Val]
        }
    def group(symbol: org.meerkat.tree.Nonterminal, p: AbstractParser[NonPackedNode]): Group = ???
  }
  
  trait Sequence extends AbstractParser[NonPackedNode] with Slot { def size: Int; def symbol: org.meerkat.tree.Sequence }
  trait Alternation extends AbstractParser[NonPackedNode] { def symbol: org.meerkat.tree.Alt }
  
  trait Terminal extends Symbol { def symbol: org.meerkat.tree.Terminal }
  
  type Action[T] = { type Value = T }
  trait AbstractNonterminal extends Symbol { def symbol: org.meerkat.tree.Nonterminal }
  
  type Nonterminal = AbstractNonterminal with Action[NoValue]
  type NonterminalWithAction[T] = AbstractNonterminal with Action[T]
  
  def epsilon[Val] = new Terminal {
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = CPSResult.success(sppfLookup.getEpsilonNode(i))
    def symbol = org.meerkat.tree.Terminal(name)
    def name = "epsilon"; override def toString = name
    type Value = Val
  }
  
  val Ø = epsilon[NoValue]
  
  trait SequenceBuilder extends (Slot => Sequence) { import AbstractParser._ 
    type Value
    def action: Option[Any => Any] = None
    
    def ~ (p: Symbol)(implicit tuple: this.Value & p.Value) = { implicit val o = obj1(tuple); seq(this, p) }
    
    def | (p: AlternationBuilder)(implicit sub: this.Value <:< p.Value) = altSeqAlt(this, p)
    def | (p: SequenceBuilder)(implicit sub: this.Value <:< p.Value) = altSeq(this, p)
    def | (p: Symbol)(implicit sub: this.Value <:< p.Value) = altSeqSym(this, p)
    
    def | (q: SequenceBuilderWithAction)(implicit sub: this.Value <:< q.Value) = altSeq(this, q)
    def | (q: SymbolWithAction)(implicit sub: this.Value <:< q.Value) = altSeqSym(this, q)
    
    def ^^[V](f: this.Value => V) = new SequenceBuilderWithAction {
      type Value = V
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      def action = Option({ x => f(x.asInstanceOf[SequenceBuilder.this.Value]) })
    }
    
    def ^[V](f: String => V)(implicit sub: this.Value <:< NoValue) = new SequenceBuilderWithAction {
      type Value = V
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      def action = Option({ x => f(x.asInstanceOf[String]) })
    }
  }
  
  trait AlternationBuilder extends (Head => Alternation) { import AbstractParser._
    type Value
    
    def | (p: AlternationBuilder)(implicit sub: this.Value <:< p.Value) = altAlt(this, p)
    def | (p: SequenceBuilder)(implicit isSubtype: this.Value <:< p.Value) = altAltSeq(this, p)
    def | (p: Symbol)(implicit isSubtype: this.Value <:< p.Value) = altAltSym(this, p)
    
    def | (q: SequenceBuilderWithAction)(implicit sub: this.Value <:< q.Value) = altAltSeq(this, q)
    def | (q: SymbolWithAction)(implicit sub: this.Value <:< q.Value) = altAltSym(this, q)
  }
  
  trait Symbol extends AbstractParser[NonPackedNode] { import AbstractParser._
    
    type Value  
    def name: String
    def action: Option[Any => Any] = None
    
    def ~ (p: Symbol)(implicit tuple: this.Value & p.Value) = { implicit val o = obj1(tuple); seq(this, p) }
    
    def | (p: AlternationBuilder)(implicit sub: this.Value <:< p.Value) = altSymAlt(this, p)
    def | (p: SequenceBuilder)(implicit sub: this.Value <:< p.Value) = altSymSeq(this, p)
    def | (p: Symbol)(implicit sub: this.Value <:< p.Value) = altSym(this, p)
    
    def | (q: SequenceBuilderWithAction)(implicit sub: this.Value <:< q.Value) = altSymSeq(this, q)
    def | (q: SymbolWithAction)(implicit sub: this.Value <:< q.Value) = altSym(this, q)
    
    def ^^[V](f: this.Value => V) = new SymbolWithAction {
      type Value = V
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = Symbol.this(input, i, sppfLookup)
      def name = Symbol.this.name; def symbol = Symbol.this.symbol
      def action = Option({ x => f(x.asInstanceOf[Symbol.this.Value]) })
    }
    
    def ^[V](f: String => V)(implicit sub: this.Value <:< NoValue) = new SymbolWithAction {
      type Value = V
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = Symbol.this(input, i, sppfLookup)
      def name = Symbol.this.name; def symbol = Symbol.this.symbol
      def action = Option({ x => f(x.asInstanceOf[String]) })
    }
    
//    var opt: Option[Nonterminal] = None
//    def ?(): Nonterminal = opt.getOrElse({ 
//      val p = regular(org.meerkat.tree.Opt(this.symbol), this | epsilon); opt = Option(p); p })
//      
    var star: Option[AbstractNonterminal { type Value = List[Symbol.this.Value] }] = None
    def *() = star.getOrElse({
      implicit val f4 = new &[List[Symbol.this.Value],Symbol.this.Value] { type R = List[Symbol.this.Value] }
      val p = regular[NonPackedNode,this.Value](org.meerkat.tree.Star(this.symbol), star.get ~ this | epsilon[List[this.Value]])
      star = Option(p); p })
//    
//    var plus: Option[AbstractNonterminal] = None
//    def +(): AbstractNonterminal = plus.getOrElse({
//      val p = regular(org.meerkat.tree.Plus(this.symbol), plus.get ~ this | this); plus = Option(p); p })
    
    def \(): AbstractNonterminal = ???
    def !>>(): AbstractNonterminal = ???
    def !<<(): AbstractNonterminal = ???
  }
  
  trait SequenceBuilderWithAction extends (Slot => Sequence) { import AbstractParser._
    type Value
    def action: Option[Any => Any]
    
    def | (p: AlternationBuilder)(implicit sub: this.Value <:< p.Value) = altSeqAlt(this, p)
    def | (p: SequenceBuilder)(implicit sub: this.Value <:< p.Value) = altSeq(this, p)
    def | (p: Symbol)(implicit sub: this.Value <:< p.Value) = altSeqSym(this, p)
    
    def | (q: SequenceBuilderWithAction)(implicit sub: this.Value <:< q.Value) = altSeq(this, q)
    def | (q: SymbolWithAction)(implicit sub: this.Value <:< q.Value) = altSeqSym(this, q)
  }
  
  trait SymbolWithAction extends AbstractParser[NonPackedNode] { import AbstractParser._
    type Value  
    def name: String
    def action: Option[Any => Any]
  
    def | (p: AlternationBuilder)(implicit sub: this.Value <:< p.Value) = altSymAlt(this, p)
    def | (p: SequenceBuilder)(implicit sub: this.Value <:< p.Value) = altSymSeq(this, p)
    def | (p: Symbol)(implicit sub: this.Value <:< p.Value) = altSym(this, p)
    
    def | (q: SequenceBuilderWithAction)(implicit sub: this.Value <:< q.Value) = altSymSeq(this, q)
    def | (q: SymbolWithAction)(implicit sub: this.Value <:< q.Value) = altSym(this, q)
  }
  
  implicit def toTerminal(s: String) 
    = new Terminal { 
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) 
          = if (input.startsWith(s, i)) { CPSResult.success(sppfLookup.getTerminalNode(s, i, i + s.length())) } 
            else CPSResult.failure
        def symbol = org.meerkat.tree.Terminal(s)
        def name = s; override def toString = name
        type Value = NoValue
      }
  
  def ntAlt[T](name: String, p: => AlternationBuilder { type Value = T }) = nonterminalAlt[NonPackedNode,T](name, p)  
  def ntSeq[T](name: String, p: => SequenceBuilder { type Value = T }) = nonterminalSeq[NonPackedNode,T](name, p)
  def ntSym(name: String, p: AbstractSymbol[NonPackedNode]) = nonterminalSym(name, p)
  
  def run(input: Input, sppf: SPPFLookup, parser: AbstractNonterminal): Unit = {
    parser(input, 0, sppf)(t => if(t.rightExtent == input.length) { println(s"Success: $t")  })
    Trampoline.run
  }
    
  object NonterminalBuilder {
    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox.Context
    
    def syn[T](p: AlternationBuilder { type Value = T }): NonterminalWithAction[T] = macro makeNonterminalAltWithName[T]
    def syn[T](p: SequenceBuilder { type Value = T }): NonterminalWithAction[T] = macro makeNonterminalSeqWithName[T]
    def syn[T](p: AbstractSymbol[NonPackedNode] { type Value = T }): NonterminalWithAction[T] = macro makeNonterminalSymWithName[T]
    
    import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName

    def makeNonterminalAltWithName[T](c: Context)(p: c.Expr[AlternationBuilder]): c.Expr[NonterminalWithAction[T]] 
      = makeCallWithName (c, "Parsers.ntAlt")
    def makeNonterminalSeqWithName[T](c: Context)(p: c.Expr[SequenceBuilder]): c.Expr[NonterminalWithAction[T]] 
      = makeCallWithName (c, "Parsers.ntSeq")
    def makeNonterminalSymWithName[T](c: Context)(p: c.Expr[AbstractSymbol[NonPackedNode]]): c.Expr[NonterminalWithAction[T]] 
      = makeCallWithName (c, "Parsers.ntSym")
  }
  
    def parse(sentence: String, parser: AbstractNonterminal): Unit = {
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
                         val x = SemanticAction.execute(node)(input)
                         println(s"WOW: $x")
                         visualize(TreeBuilder.build(node)(input), "sppf")
                         println("Done!")
    }
  }
  
}