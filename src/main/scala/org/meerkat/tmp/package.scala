package org.meerkat

/**
 * @author Anastasia Izmaylova
 */
import org.meerkat.util._
import org.meerkat.util.Input
import org.meerkat.util.visualization._
import org.meerkat.sppf.SPPFLookup
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.sppf.SemanticAction
import org.meerkat.sppf.TreeBuilder
import org.meerkat.sppf.NonPackedNode
import org.meerkat.meerkat.ParseError
import org.meerkat.meerkat.ParseSuccess
import org.meerkat.meerkat.ParseStatistics

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
  
  type &[A <: { type Abstract[_] },T] = A#Abstract[T]
  
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
  
  type Prec = (Int, Int)
  val $: Prec = (0,0)
  
  trait Layout { def get: Parsers.Symbol { type Value = NoValue } }
  def layout(p: Parsers.Symbol { type Value = NoValue }): Layout = new Layout {
    def get = p
  }
  
  def start[T](p: Parsers.Symbol { type Value = T})(implicit layout: Layout): Parsers.AbstractNonterminal { type Value = T } 
    = Parsers.ntSeq(s"start[${p.name}]", layout.get ~~ p ~~ layout.get)
  
  object DefaultLayout {
    implicit val L: Layout = layout(Parsers.ntSym("L",Parsers.toTerminal(org.meerkat.util.JavaTokens.Layout)))  
  }
  
  def run[T](input: Input, sppf: SPPFLookup, parser: AbstractCPSParsers.AbstractParser[T]): Unit = {
    parser(input, 0, sppf)(t => {})
    Trampoline.run
  }
  
  def parse[Val](sentence: String, parser: OperatorParsers.AbstractOperatorNonterminal[Val]): Unit 
    = parse(sentence, parser((0,0)))
  
  def parse[T](sentence: String, parser: AbstractCPSParsers.AbstractSymbol[T]): Unit = {
    val input = new Input(sentence)
    val sppf = new DefaultSPPFLookup(input)
    
    run(input, sppf, parser)
    
    println(s"Trying to find: ${parser.name}(0,${sentence.length()})")
    val startSymbol = sppf.getStartNode(parser, 0, sentence.length())
    
//    println("Resetting ...")
//    parser.reset
    
    startSymbol match {
      case None       => println("Parse error")
      case Some(node) => println("Success: " + node)
                         println(sppf.countAmbiguousNodes + ", " + sppf.countIntermediateNodes + ", " + sppf.countPackedNodes + ", " + sppf.countNonterminalNodes + ", " + sppf.countTerminalNodes)
                         println("Visualizing...")
                         visualize(node, input)
                         val x = SemanticAction.execute(node)(input)
                         println(s"WOW: $x")
                         visualize(TreeBuilder.build(node)(input), input)
                         println("Done!")
    }
  }
  
  def parse(parser: Parsers.AbstractNonterminal, input: Input): Either[ParseError, ParseSuccess] = {

    parser.reset
    
    val sppf = new DefaultSPPFLookup(input)
    
    val startUserTime = getUserTime
    val startSystemTime = getCpuTime
    val startNanoTime = System.nanoTime
    
    run(input, sppf, parser)
    
    val endUserTime: Long = getUserTime
    val endSystemTime = getCpuTime
    val endNanoTime: Long = System.nanoTime
    
    val startSymbol = sppf.getStartNode(parser, 0, input.length)
    
      startSymbol match {
        case None    => Left(ParseError(0, " "))
        case Some(x) => Right(ParseSuccess(x, ParseStatistics((endNanoTime - startNanoTime) / 1000000, 
                                                              (endUserTime - startUserTime) / 1000000,
                                                              (1000000) / 1000000,
                                                              sppf.countNonterminalNodes,
                                                              sppf.countIntermediateNodes,
                                                              sppf.countTerminalNodes,
                                                              sppf.countPackedNodes,
                                                              sppf.countAmbiguousNodes)))
  }
  }
  
}