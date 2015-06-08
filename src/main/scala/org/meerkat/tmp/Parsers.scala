package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.util.Input
import org.meerkat.sppf.SPPFLookup
import scala.reflect.ClassTag
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.sppf.Slot
import org.meerkat.tree.RuleType
import scala.util.matching.Regex
import scala.collection.mutable._
import scala.collection.JavaConversions._

object Parsers { import AbstractCPSParsers._
  
  implicit def obj1[ValA,ValB](implicit vals: ValA|~|ValB) = new CanBuildSequence[NonPackedNode,NonPackedNode,ValA,ValB] {
    implicit val m1 = obj4; implicit val m2 = obj4
    
    type T = NonPackedNode; type V = vals.R
      
    type Sequence = Parsers.Sequence
    def sequence(p: AbstractSequence[NonPackedNode]): Sequence 
      = new Sequence { 
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input,i,sppfLookup)
          def size = p.size; def symbol = p.symbol; def ruleType = p.ruleType
          override def reset = p.reset
        }  
    def index(a: T): Int = a.rightExtent
    def intermediate(a: T, b: T, p: Slot, sppfLookup: SPPFLookup): T = sppfLookup.getIntermediateNode(p, a, b)
      
    type SequenceBuilder = Parsers.SequenceBuilder[V]
    def builderSeq(f: Slot => Sequence) = new Parsers.SequenceBuilder[V] { def apply(slot: Slot) = f(slot) }
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
          override def reset = p.reset
        }   
    type AlternationBuilder = Parsers.AlternationBuilder[ValB]
    def builderAlt(f: Head => Alternation) = new Parsers.AlternationBuilder[ValB] { def apply(head: Head) = f(head) }
  }
  
  implicit object obj4 extends Memoizable[NonPackedNode] {
    type U = Int
    def value(t: NonPackedNode): Int = t.rightExtent
  }
  
  implicit def obj5[Val] = new CanBuildNonterminal[NonPackedNode,Val] {
    implicit val m = obj4
    
    type Nonterminal = Parsers.AbstractNonterminal[Val]
    def nonterminal(nt: String, p: AbstractParser[NonPackedNode]) 
      = new Parsers.AbstractNonterminal[Val] {
          type Value = Val
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def symbol = org.meerkat.tree.SimpleNonterminal(nt)
          def name = nt; override def toString = name
          override def reset = p.reset
        }
    
    type Symbol = Parsers.Symbol[Val]
    def symbol(p: AbstractSymbol[NonPackedNode,Val]) = new Parsers.Symbol[Val] { 
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input,i,sppfLookup)
      def name = p.name; def symbol = p.symbol
      override def reset = p.reset
    }
  }
  
  implicit def obj6[Val] = new CanBuildEBNF[NonPackedNode,Val] {
    implicit val m = obj4
    
    type T = NonPackedNode
    type Regular = AbstractNonterminal[Val]
    type Group = AbstractNonterminal[Val]
    
    def regular(sym: org.meerkat.tree.Nonterminal, p: AbstractParser[NonPackedNode]): Regular 
      = new AbstractNonterminal[Val] {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def name = symbol.toString; def symbol = sym
          override def toString = name   
          override def reset = p.reset
        }
    def group(p: AbstractParser[NonPackedNode]): Group 
      = new AbstractNonterminal[Val] {
          def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
          def name = symbol.toString; def symbol = org.meerkat.tree.Group(p.symbol)
          override def toString = name   
          override def reset = p.reset
        }     
  }
  
  trait Sequence extends AbstractParser[NonPackedNode] with Slot { def size: Int; def symbol: org.meerkat.tree.Sequence }
  trait Alternation extends AbstractParser[NonPackedNode] { def symbol: org.meerkat.tree.Alt }
  
  trait AbstractNonterminal[+V] extends Symbol[V] { def symbol: org.meerkat.tree.Nonterminal; type Abstract[+X] = AbstractNonterminal[X] }
  type Nonterminal = AbstractNonterminal[NoValue]
  
  trait Terminal extends Symbol[NoValue] { def symbol: org.meerkat.tree.Terminal }
  
  val Ø = new Terminal {
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = CPSResult.success(sppfLookup.getEpsilonNode(i))
    def symbol = org.meerkat.tree.Terminal(name)
    def name = "epsilon"; override def toString = name
  }
  
  trait SequenceBuilder[+V] extends (Slot => Sequence) with SequenceBuilderOps[V] { import AbstractParser._ 
    def action: Option[Any => V] = None
    
    def ~ [U](p: Symbol[U])(implicit tuple: V|~|U, layout: Layout) = (this ~~ layout.get).~~(p)(tuple)
    def ~~ [U](p: Symbol[U])(implicit tuple: V|~|U) = seq(this, p)(obj1(tuple))
  
//    def ~ (p: String)(implicit layout: Layout) = (this ~~ layout.get).~~(p)
//    def ~~ (p: String)(implicit tuple: this.Value|~|NoValue) = { implicit val o = obj1(tuple); seq(this, p) }
    
    def & [U](f: V => U) = new SequenceBuilderWithAction[U] {
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      def action = Option({ x => f(x.asInstanceOf[V]) })
    }
    
    def ^[U](f: String => U)(implicit sub: V <:< NoValue) = new SequenceBuilderWithAction[U] {
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      def action = Option({ x => f(x.asInstanceOf[String]) })
    }
    
    var group: Option[AbstractNonterminal[_]] = None
    def !(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.Group] = {
      type T = AbstractNonterminal[ebnf.Group]
      group.asInstanceOf[Option[T]].getOrElse({ 
        val p = groupSeq(this); group = Option(p); p })
    }
    
    def ~ [U](q: OperatorParsers.AbstractOperatorNonterminal[U])(implicit tuple: V|~|U, layout: Layout) = (this ~~ layout.get).~~(q)(tuple)
    def ~~ [U](q: OperatorParsers.AbstractOperatorNonterminal[U])(implicit tuple: V|~|U) 
      = AbstractOperatorParsers.AbstractOperatorParser.seqSeqNt(this, q)(OperatorParsers.OperatorImplicits.obj1[V,U](tuple))
  }
  
  trait SequenceBuilderWithAction[+V] extends (Slot => Sequence) with SequenceBuilderOps[V] { import AbstractParser._
    def action: Option[Any => V]
  }
  
  trait SequenceBuilderOps[+V] extends (Slot => Sequence) { import AbstractParser._
    def action: Option[Any => V]
    
    def | [U >: V](p: AlternationBuilder[U]) = altSeqAlt(this, p)
    def | [U >: V](p: SequenceBuilder[U]) = altSeq(this, p)
    def | [U >: V](p: Symbol[U]) = altSeqSym(this, p)
    
    def | [U >: V](q: SequenceBuilderWithAction[U]) = altSeq(this, q)
    def | [U >: V](q: SymbolWithAction[U]) = altSeqSym(this, q)
  }
  
  trait AlternationBuilder[+V] extends (Head => Alternation) { import AbstractParser._
    def action: Option[Any => V] = None
    
    def | [U >: V](p: AlternationBuilder[U]) = altAlt(this, p)
    def | [U >: V](p: SequenceBuilder[U]) = altAltSeq(this, p)
    def | [U >: V](p: Symbol[U]) = altAltSym(this, p)
    
    def | [U >: V](q: SequenceBuilderWithAction[U]) = altAltSeq(this, q)
    def | [U >: V](q: SymbolWithAction[U]) = altAltSym(this, q)
    
    var group: Option[AbstractNonterminal[_]] = None
    def !(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.Group] = {
      type T = AbstractNonterminal[ebnf.Group]
      group.asInstanceOf[Option[T]].getOrElse({ val p = groupAlt(this); group = Option(p); p })
    }
  }
  
  trait Symbol[+V] extends AbstractParser[NonPackedNode] with SymbolOps[V] with EBNFs[V] with CharLevelDisambiguation[V] { import AbstractParser._    
    def name: String
    def action: Option[Any => V] = None
    
    def ~ [U](p: Symbol[U])(implicit tuple: V|~|U, layout: Layout) = (this ~~ layout.get).~~(p)(tuple)
    def ~~ [U](p: Symbol[U])(implicit tuple: V|~|U) = seq(this, p)(obj1(tuple))
  
    def ~ (p: String)(implicit layout: Layout) = this ~~ layout.get ~~ p
    def ~~ (p: String)(implicit tuple: V|~|NoValue) = seq(this, p)(obj1(tuple))
  
    def &[U](f: V => U) = new SymbolWithAction[U] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = Symbol.this(input, i, sppfLookup)
      def name = Symbol.this.name; def symbol = Symbol.this.symbol
      def action = Option({ x => f(x.asInstanceOf[V]) })
      override def reset = Symbol.this.reset
    }   
    def ^[U](f: String => U)(implicit sub: V <:< NoValue) = new SymbolWithAction[U] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = Symbol.this(input, i, sppfLookup)
      def name = Symbol.this.name; def symbol = Symbol.this.symbol
      def action = Option({ x => f(x.asInstanceOf[String]) })
      override def reset = Symbol.this.reset
    }
  }
  
  trait SymbolWithAction[+V] extends AbstractParser[NonPackedNode] with SymbolOps[V] { import AbstractParser._
    def name: String
    def action: Option[Any => V]
  }
  
  trait SymbolOps[+V] extends AbstractParser[NonPackedNode] { import AbstractParser._
    def name: String
    def action: Option[Any => V]
  
    def | [U >: V](p: AlternationBuilder[U]) = altSymAlt(this, p)
    def | [U >: V](p: SequenceBuilder[U]) = altSymSeq(this, p)
    def | [U >: V](p: Symbol[U]) = altSym(this, p)
    
    def | [U >: V](q: SequenceBuilderWithAction[U]) = altSymSeq(this, q)
    def | [U >: V](q: SymbolWithAction[U]) = altSym(this, q)
  }
  
  implicit def toTerminal(s: String): Terminal = new Terminal { 
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup) 
      = if (input.startsWith(s, i)) CPSResult.success(sppfLookup.getTerminalNode(s, i, i + s.length())) 
        else CPSResult.failure
    def symbol = org.meerkat.tree.Terminal(s); def name = s; override def toString = name
  }
  
  implicit def toTerminal(r: Regex) = new Terminal {
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = { 
      val end = input.matchRegex(r, i)
      if(end != -1) CPSResult.success(sppfLookup.getTerminalNode(r.toString, i, end))
      else CPSResult.failure 
    }
    def name = r.toString; def symbol = org.meerkat.tree.Terminal(name)
  }
  
  implicit def toTerminal(r: org.meerkat.util.RegularExpression) = new Terminal {
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = { 
      val end = r.matcher.next(input, i)
      if(end != -1) CPSResult.success(sppfLookup.getTerminalNode(r.toString, i, end))
      else CPSResult.failure 
    }
    def name = r.toString; def symbol = org.meerkat.tree.Terminal(name)
  }
  
  def ntAlt[Val](name: String, p: => AlternationBuilder[Val]) = nonterminalAlt(name, p)  
  def ntSeq[Val](name: String, p: => SequenceBuilder[Val]) = nonterminalSeq(name, p)
  def ntSym[Val](name: String, p: => AbstractSymbol[NonPackedNode,Val]) = nonterminalSym(name, p)
  
  trait EBNFs[+V] { self: Symbol[V] =>   
    var opt: Option[AbstractNonterminal[_]] = None
    def ?(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      opt.asInstanceOf[Option[T]].getOrElse({
        val p = regular[NonPackedNode,ebnf.OptOrSeq](org.meerkat.tree.Opt(this.symbol), this & ebnf.unit | Ø ^ ebnf.empty) 
        opt = Option(p); p })
    }
    
    var star: Option[AbstractNonterminal[_]] = None
    def *(implicit ebnf: EBNF[V], layout: Layout): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      star.asInstanceOf[Option[T]].getOrElse({
        val p = regular[NonPackedNode,ebnf.OptOrSeq](org.meerkat.tree.Star(this.symbol), star.asInstanceOf[Option[T]].get ~ this & ebnf.add | Ø ^ ebnf.empty)
        star = Option(p); p })
    }
    
    var starstar: Option[AbstractNonterminal[_]] = None
    def **(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      starstar.asInstanceOf[Option[T]].getOrElse({
        val p = regular[NonPackedNode,ebnf.OptOrSeq](org.meerkat.tree.Star(this.symbol), starstar.asInstanceOf[Option[T]].get ~~ this & ebnf.add | Ø ^ ebnf.empty)
        starstar = Option(p); p })
    }
    
    var star_sep: Map[String,AbstractNonterminal[_]] = new java.util.HashMap[String,AbstractNonterminal[_]]()
    def *(sep: Terminal)(implicit ebnf: EBNF[V], layout: Layout): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      star_sep.getOrElseUpdate(sep.name, {
        regular[NonPackedNode,ebnf.OptOrSeq](org.meerkat.tree.Star(this.symbol), this.+(sep)(ebnf,layout) | Ø ^ ebnf.empty)
      }).asInstanceOf[T]
    }
          
    var plus: Option[AbstractNonterminal[_]] = None
    def +(implicit ebnf: EBNF[V], layout: Layout): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      plus.asInstanceOf[Option[T]].getOrElse({ 
        val p = regular[NonPackedNode,ebnf.OptOrSeq](org.meerkat.tree.Plus(this.symbol), plus.asInstanceOf[Option[T]].get ~ this & ebnf.add | this & ebnf.unit)
        plus = Option(p); p })
    }
    
    var plusplus: Option[AbstractNonterminal[_]] = None
    def ++(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      plusplus.asInstanceOf[Option[T]].getOrElse({ 
        val p = regular[NonPackedNode,ebnf.OptOrSeq](org.meerkat.tree.Plus(this.symbol), plusplus.asInstanceOf[Option[T]].get ~~ this & ebnf.add | this & ebnf.unit)
        plusplus = Option(p); p })
    }
    
    var plus_sep: Map[String,AbstractNonterminal[_]] = new java.util.HashMap[String,AbstractNonterminal[_]]()
    def +(sep: Terminal)(implicit ebnf: EBNF[V], layout: Layout): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      plus_sep.getOrElseUpdate(sep.name, {
        regular[NonPackedNode,ebnf.OptOrSeq](org.meerkat.tree.Plus(this.symbol), plus_sep.get(sep.name).get.asInstanceOf[T] ~ sep ~ this & ebnf.add | this & ebnf.unit)
      }).asInstanceOf[T]
    }
  }
  
  trait CharLevelDisambiguation[+V] { self: Symbol[V] =>
    def \ (arg: String) = postFilter(this, (input,t:NonPackedNode) => arg != input.substring(t.leftExtent, t.rightExtent), s" \\ $arg")
    def \ (args: Set[String]) = postFilter(this, (input,t:NonPackedNode) => !args.contains(input.substring(t.leftExtent, t.rightExtent)), " \\ " + args.mkString(","))
    def \ (args: String*) = postFilter(this, (input,t:NonPackedNode) => !args.contains(input.substring(t.leftExtent, t.rightExtent)), " \\ " + args.mkString(","))
    def \ (arg: Regex) = postFilter(this, (input,t:NonPackedNode) => !input.matchRegex(arg, t.leftExtent, t.rightExtent), s" \\ $arg")
    def \ (arg: Char) = postFilter(this, (input,t:NonPackedNode) => !(t.rightExtent - t.leftExtent == 1 && input.charAt(t.leftExtent) == arg), s" \\ $arg")
    
    def !>>(arg: String) = postFilter(this, (input,t:NonPackedNode) => !input.startsWith(arg, t.rightExtent), s" !>> $arg")
    def !>>(args: String*) = postFilter(this, (input,t:NonPackedNode) => !args.exists(input.startsWith(_, t.rightExtent)), " !>> " + args.mkString(","))
    def !>>(arg: Regex) = postFilter(this, (input,t:NonPackedNode) => input.matchRegex(arg, t.rightExtent) == -1, s" !>> $arg")
    def !>>(arg: Char) = postFilter(this, (input,t:NonPackedNode) => input.charAt(t.rightExtent) != arg, s" !>> $arg")
    
    def !<<(arg: String) = preFilter(this, (input,i) => !input.substring(0,i).endsWith(arg), s"$arg !<< ")
    def !<<(args: String*) = preFilter(this, (input,i) => { val sub = input.substring(0, i); args.filter(sub.endsWith(_)).isEmpty }, args.mkString(",") + " !<< ")
    def !<<(arg: Regex) = preFilter(this, (input, i) => !input.matchRegex(arg, i - 1, i), s"$arg !<< ")
    def !<<(arg: Char) = preFilter(this, (input, i) => !(i > 0 && input.charAt(i - 1) == arg), s"$arg !<< ")
  }
}