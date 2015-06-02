package org.meerkat

/**
 * @author Anastasia Izmaylova
 */
import org.meerkat.util.Input
import org.meerkat.util.visualization._
import org.meerkat.sppf.SPPFLookup
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.sppf.SemanticAction
import org.meerkat.sppf.TreeBuilder
import org.meerkat.sppf.NonPackedNode

package object tmp {
  
  case class ~[+A,+B](_1: A, _2: B)
  
  trait <:<[A,B]
  implicit def sub[A,B >: A]: A <:< B = null
  
  trait <:!<[A,B]
  implicit def nsub[A,B]: A <:!< B = null
  implicit def nsubAmb1[A,B >: A]: A <:!< B = null
  implicit def nsubAmb2[A,B >: A]: A <:!< B = null

  type ![T] = { type f[U] = U <:!< T }
  
  sealed trait NoValue
  
  trait |~|[A,B] { type R }
  implicit def f1[A <: NoValue,B <: NoValue] = new |~|[NoValue,NoValue] { type R = NoValue }
  implicit def f2[A <: NoValue,B: ![NoValue]#f] = new |~|[NoValue,B] { type R = B }
  implicit def f3[A: ![NoValue]#f,B <: NoValue] = new |~|[A,NoValue] { type R = A }
  implicit def f4[A: ![NoValue]#f,B: ![NoValue]#f] = new |~|[A,B] { type R = (A,B) }
  
  trait EBNF[Val] {
    type OptOrSeq; type Seq; type Group
    val add: ((OptOrSeq,Val)) => OptOrSeq
    val unit: Val => OptOrSeq
    val empty: String => OptOrSeq
    val group: Val => Group
  }
  
  implicit val ebnf1 = new EBNF[NoValue] { 
    type OptOrSeq = NoValue; type Group = NoValue
    val add: ((OptOrSeq,NoValue)) => OptOrSeq = _ => null
    val unit: NoValue => OptOrSeq = _ => null
    val empty: String => OptOrSeq = _ => null
    val group: NoValue => Group = _ => null
  }
  implicit def ebnf2[Val: ![NoValue]#f] = new EBNF[Val] { 
    type OptOrSeq = List[Val]; type Group = Val
    val add: ((OptOrSeq,Val)) => OptOrSeq = { case (s,x) => s.:+(x) }
    val unit: Val => OptOrSeq = x => List(x)
    val empty: String => OptOrSeq = _ => List()
    val group: Val => Group = x => x
  }
  
  trait Layout { def get: Parsers.Symbol { type Value = NoValue } }
  def layout(p: Parsers.Symbol { type Value = NoValue }): Layout = new Layout {
    val q = Parsers.ntSym("L", p)
    def get = q
  }
  implicit val default: Layout = layout(Parsers.toTerminal(org.meerkat.util.JavaTokens.Layout))
  
  object Syntax {
    import AbstractCPSParsers._
    import Parsers._
    import OperatorParsers._
    
    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox.Context
    
    import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName
    
    def syn[T](p: AlternationBuilder { type Value = T }) = macro makeNonterminalAltWithName[T]
    def syn[T](p: SequenceBuilder { type Value = T }) = macro makeNonterminalSeqWithName[T]
    def syn[T](p: AbstractSymbol[NonPackedNode] { type Value = T }) = macro makeNonterminalSymWithName[T]
    
    def makeNonterminalAltWithName[T](c: Context)(p: c.Expr[AlternationBuilder]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntAlt")
    def makeNonterminalSeqWithName[T](c: Context)(p: c.Expr[SequenceBuilder]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntSeq")
    def makeNonterminalSymWithName[T](c: Context)(p: c.Expr[AbstractSymbol[NonPackedNode]]): c.Expr[Nonterminal & T] = makeCallWithName (c, "Parsers.ntSym")
    
    def syn[T](p: OperatorAlternationBuilder[T]) = macro makeOperatorNonterminalAltWithName[T]
    def syn[T](p: OperatorSequenceBuilder[T]) = macro makeOperatorNonterminalSeqWithName[T]
    def syn[T](p: AbstractOperatorNonterminal[T]) = macro makeOperatorNonterminalSymWithName[T]
    def syn[T](p: OperatorSequenceBuilderWithAction[T]) = macro makeOperatorNonterminalSeqWithActionWithName[T]
    def syn[T](p: OperatorNonterminalWithAction[T]) = macro makeOperatorNonterminalSymWithActionWithName[T]
    
    def makeOperatorNonterminalAltWithName[T](c: Context)(p: c.Expr[OperatorAlternationBuilder[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntAlt")
    def makeOperatorNonterminalSeqWithName[T](c: Context)(p: c.Expr[OperatorSequenceBuilder[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntSeq")
    def makeOperatorNonterminalSymWithName[T](c: Context)(p: c.Expr[AbstractOperatorNonterminal[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntSym")
    def makeOperatorNonterminalSeqWithActionWithName[T](c: Context)(p: c.Expr[OperatorSequenceBuilderWithAction[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntSeqWithAction")
    def makeOperatorNonterminalSymWithActionWithName[T](c: Context)(p: c.Expr[OperatorNonterminalWithAction[T]]): c.Expr[OperatorNonterminal & T] 
      = makeCallWithName (c, "OperatorParsers.ntSymWithAction")
  }
  
  def run(input: Input, sppf: SPPFLookup, parser: AbstractCPSParsers.AbstractParser[NonPackedNode]): Unit = {
    parser(input, 0, sppf)(t => if(t.rightExtent == input.length) { println(s"Success: $t")  })
    Trampoline.run
  }
  
  def parse[Val](sentence: String, parser: OperatorParsers.AbstractOperatorNonterminal[Val]): Unit 
    = parse(sentence, parser((0,0)))
  
  def parse(sentence: String, parser: Parsers.AbstractNonterminal): Unit = {
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