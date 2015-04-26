/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package meerkat

import scala.collection.mutable._
import sppf._
import util._
import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.util.matching.Regex
import util.JavaTokens._
import scala.collection.JavaConversions._
import Result._
import Rule._

trait Layout { def parser: MeerkatParser }

trait MeerkatParser extends Parser {
  
  import MeerkatLogging._
  
  var name: Lazy[String] = this.hashCode().toString
  def nameAs(name: => String): Unit = this.name = new Lazy(name)
  override def toString: String = this.name.value
    
  var head: String = ""
  def head(head: String): Unit = this.head = head
  def headed: Boolean = !head.isEmpty()
    
  var seq = false
  def sequenced: Boolean = seq
  def sequence: Unit = seq = true
    
  var alt = false
  def alternated: Boolean = alt
  def alternate: Unit = alt = true
    
  def groupSeqOrAlt: MeerkatParser = if(this.sequenced || this.alternated) this.gr else this
  def groupAlt: MeerkatParser = if(this.alternated) this.gr else this
    
  var reset: Unit => Unit = { _ => }
  def resetWith(f: => Unit): Unit = reset = { _ => f }
    
  /**
   * Filter combinators
   */
  def postFilter(p: (Input, NonPackedNode) => Boolean): MeerkatParser 
    = new MeerkatParser {
        def apply(input: Input, sppf: SPPFLookup, i: Int) = MeerkatParser.this(input, sppf, i).filter(t => p(input, t))
      }
    
  def preFilter(p: (Input, Int) => Boolean): MeerkatParser
    = new MeerkatParser {
        def apply(input: Input, sppf: SPPFLookup, i: Int) = if(p(input, i)) MeerkatParser.this(input, sppf, i) else failure
      }
    
  /**
   * Sequence and alternation combinators
   */
    
  def ~(seq2: MeerkatParser)(implicit l: Layout): MeerkatParser = this ~~ l.parser ~~ seq2
    
  def ~~(seq2: MeerkatParser): MeerkatParser = {
    val p1 = this.groupAlt
    val p2 = seq2.groupSeqOrAlt 
      
    val p = new MeerkatParser { 
              def apply(input: Input, sppf: SPPFLookup, i: Int) 
                = if(this.headed) {
                    p1(input, sppf, i).flatMap(t1 => { inSequence(t1, p2.name.value)
                      p2(input, sppf, t1.rightExtent).mapNoMemo(t2 => {
                        val r = sppf.getNonterminalNode(this.head, this, Some(t1), t2); endOfAlt(this.head, this.name.value, i, r)
                        r }) })
                  } else {
                    p1(input, sppf, i).flatMap(t1 => { inSequence(t1, p2.name.value)
                      p2(input, sppf, t1.rightExtent).mapNoMemo(t2 => 
                        sppf.getIntermediateNode(this, t1, t2)) })
                  }
              }  
    p.nameAs((if(p.headed) p.head + " ::= " else "") + p1.name.value + p2.name.value + p.hashCode() + "@")
    p.sequence
    p.resetWith({ p1.reset(); p2.reset() })
    p
  }
    
  def |(alt2: MeerkatParser): MeerkatParser = {
    var p: MeerkatParser = null
    val p1: Lazy[MeerkatParser] = this.rule(p.head)
    val q2: MeerkatParser = alt2.groupAlt
    val p2: Lazy[MeerkatParser] = q2.rule(p.head)
      
    p = new MeerkatParser { def apply(input: Input, sppf: SPPFLookup, i: Int) = p1.value(input, sppf, i) orElse p2.value(input, sppf, i) }
    p.nameAs(this.name.value + " | " + q2.name.value)
    p.alternate
    p.resetWith({ p1.value.reset(); p2.value.reset() })
    p
  }
    
  def rule(h: String): MeerkatParser 
    = if(this.alternated) {
        this.head(h); this
      } else {
        val p = if(this.sequenced) {
                  this.head(h)
                  new MeerkatParser {
                    def apply(input: Input, sppf: SPPFLookup, i: Int) = { beginOfAlt(h, this.name.value, i)
                      MeerkatParser.this(input, sppf, i)
                    }
                  }
                } else
                  new MeerkatParser {
                    def apply(input: Input, sppf: SPPFLookup, i: Int) = { beginOfAlt(h, this.name.value, i)
                      MeerkatParser.this(input, sppf, i).map(t => { endOfAlt(h, this.name.value, i, t)
                        sppf.getNonterminalNode(h, this, t) })
                    }
                  }
        if(this.sequenced) p.nameAs(this.name.value)  
        else p.nameAs(h + " ::= " + this.name.value + p.hashCode())
        p.resetWith(this.reset())
        p
      }
    
  /**
   * A?
   */
  var option: Option[MeerkatParser] = None
  def ?(): MeerkatParser = {
    option.getOrElse({
      val p: MeerkatParser = this.name.value + "?" ::= this | epsilon
      option = Option(p)
      p
    })
  }
    
  /**
   * A*
   */
  var star: Option[MeerkatParser] = None
  def *(implicit l: Layout): MeerkatParser = {
    star.getOrElse({
      val p: MeerkatParser = this.name.value + "*" ::= this.+ | epsilon
      star = Option(p)
      p
    })
  }
    
  /**
   * A**
   */
  var star_no_layout: Option[MeerkatParser] = None
  def **(): MeerkatParser = {
    star_no_layout.getOrElse({
      val p: MeerkatParser = this.name.value + "**" ::= star_no_layout.get ~~ this | epsilon
      star_no_layout = Option(p)
      p
    })
  }
    
  /**
   * A* - fixed length
   */
  var star_fixed_length: Map[Int, MeerkatParser] = new java.util.HashMap[Int, MeerkatParser]()
  def *(n: Int)(implicit l: Layout): MeerkatParser
    = star_fixed_length.getOrElseUpdate(n, 
        this.name.value + "*" + "(" + n + ")" ::= (if(n == 0) epsilon else (2 to n).foldLeft(this)
                                                                                      ((p, _) => p ~ this)))
    
  /**
   * A+
   */
  var plus: Option[MeerkatParser] = None
  def +(implicit l: Layout): MeerkatParser = {
    plus.getOrElse({
      val p: MeerkatParser = this.name.value + "+" ::= plus.get ~ this | this
      plus = Option(p)
      p
    })
  }
    
  /**
   * A++
   */
  var plus_no_layout: Option[MeerkatParser] = None
  def ++(): MeerkatParser = {
    plus_no_layout.getOrElse({
      val p: MeerkatParser = this.name.value + "++" ::= plus_no_layout.get ~~ this | this
      plus_no_layout = Option(p)
      p
    })
  }
    
  /**
   * {A separator}*
   */
  val star_sep: Map[String, MeerkatParser] = new java.util.HashMap[String, MeerkatParser]()
  def *(sep: TerminalParser)(implicit l: Layout): MeerkatParser = {
    star_sep.getOrElseUpdate(sep.toString, {
      val p: MeerkatParser = "{" + this.name.value + " " + sep + "}*" ::= this.+(sep) | epsilon
      p
    })
  }
    
  /**
   * {A separator}+
   */
  val plus_sep: Map[String, MeerkatParser] = new java.util.HashMap[String, MeerkatParser]()
  def +(sep: TerminalParser)(implicit l: Layout): MeerkatParser = {
    plus_sep.getOrElseUpdate(sep.toString, {
      val p: MeerkatParser = "{" + this.name.value + " " + sep + "}+" ::= plus_sep.get(sep.toString).get ~ sep ~ this | this // 'plus_sep.get(separator).get' should work due to pass-by-name
      p
    })
  }
    
  /**
   * A \ "a"
   */
  val diff: Map[Any, MeerkatParser] = new java.util.HashMap[Any, MeerkatParser]()
  def \(args: Set[String]): MeerkatParser 
    = { val p = this.groupSeqOrAlt
        val q = p.postFilter((input, t) => !args.contains(input.substring(t.leftExtent, t.rightExtent))); q.nameAs(p.name.value); q.resetWith(p.reset()); q }
    
  def \(arg: String): MeerkatParser 
    = diff.getOrElseUpdate(Seq(arg), 
        { val p = this.groupSeqOrAlt
          val q = p.postFilter((input, t) => arg != input.substring(t.leftExtent, t.rightExtent)); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
                                            
  def \(arg: Char): MeerkatParser 
    = diff.getOrElseUpdate(Seq(arg.toString), 
        { val p = this.groupSeqOrAlt
          val q = p.postFilter((input, t) => !(t.rightExtent - t.leftExtent == 1 && input.charAt(t.leftExtent) == arg)); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
          
  def \(args: Char*): MeerkatParser 
    = diff.getOrElseUpdate(args, 
        { val p = this.groupSeqOrAlt
          val q = p.postFilter((input, t) => !args.exists(arg => t.rightExtent - t.leftExtent == 1 && input.charAt(t.leftExtent) == arg)); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
        
  val diff_regex: Map[Regex, MeerkatParser] = new java.util.HashMap[Regex, MeerkatParser]()
  def \(arg: Regex): MeerkatParser =
    diff_regex.getOrElseUpdate(arg, 
        { val p = this.groupSeqOrAlt
          val q = p.postFilter((input, t) => !input.matchRegex(arg, t.leftExtent, t.rightExtent)); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
    
  /**
   * A !>> "a"
   */
  val not_follow: Map[scala.collection.Seq[String], MeerkatParser] = new java.util.HashMap[scala.collection.Seq[String], MeerkatParser]()
  def !>>(args: String*): MeerkatParser
    = not_follow.getOrElseUpdate(args, 
        { val p = this.groupSeqOrAlt
          val q = p.postFilter((input, t) => !args.exists(arg => input.startsWith(arg, t.rightExtent))); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
    
  def !>>(arg: String): MeerkatParser
    = not_follow.getOrElseUpdate(Seq(arg), 
        { val p = this.groupSeqOrAlt
          val q = p.postFilter((input, t) => !input.startsWith(arg, t.rightExtent)); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
    
  def !>>(arg: Char): MeerkatParser
    = not_follow.getOrElseUpdate(Seq(arg.toString), 
        { val p = this.groupSeqOrAlt
          val q = p.postFilter((input, t) => input.charAt(t.rightExtent) != arg); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
                
  val not_follow_regex: Map[Regex, MeerkatParser] = new java.util.HashMap[Regex, MeerkatParser]()
  def !>>(arg: Regex): MeerkatParser =
    not_follow_regex.getOrElseUpdate(arg, 
        { val p = this.groupSeqOrAlt
          val q = p.postFilter((input, t) => input.matchRegex(arg, t.rightExtent) == -1); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
          
  var not_follow_any: Option[MeerkatParser] = None
  def !>>>(): MeerkatParser =
    not_follow_any.getOrElse( 
        { val p = this.groupSeqOrAlt
          val q = p.postFilter((input, t) => t.rightExtent < input.length); q.nameAs(p.name.value); q.resetWith(p.reset());
          not_follow_any = Option(q)
          q })
    
  /**
   * A !<< "a"
   */
  val not_procede: Map[scala.collection.Seq[String], MeerkatParser] = new java.util.HashMap[scala.collection.Seq[String], MeerkatParser]()
  def !<<(args: String*): MeerkatParser
    = not_procede.getOrElseUpdate(args, 
        { val p = this.groupSeqOrAlt
          val q = p.preFilter((input, i) => { val sub = input.substring(0, i); args.filter(arg => sub.endsWith(arg)).isEmpty }); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
    
  def !<<(arg: String): MeerkatParser
    = not_procede.getOrElseUpdate(Seq(arg), 
        { val p = this.groupSeqOrAlt
          val q = p.preFilter((input, i) => !input.substring(0, i).endsWith(arg)); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
    
  def !<<(arg: Char): MeerkatParser
    = not_procede.getOrElseUpdate(Seq(arg.toString), 
        { val p = this.groupSeqOrAlt
          val q = p.preFilter((input, i) => !(i > 0 && input.charAt(i - 1) == arg)); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
    
  val not_procede_regex: Map[Regex, MeerkatParser] = new java.util.HashMap[Regex, MeerkatParser]()
  def !<<(arg: Regex): MeerkatParser
    = not_procede_regex.getOrElseUpdate(arg, 
        { val p = this.groupSeqOrAlt
          val q = p.preFilter((input, i) => !input.matchRegex(arg, i - 1, i)); q.nameAs(p.name.value); q.resetWith(p.reset()); q })
    
  /**
   * (A)
   */
  var group: Option[MeerkatParser] = None
  def gr(): MeerkatParser = {
    group.getOrElse({
      val p: MeerkatParser = "(" + this.name.value + ")" ::= this
      group = Option(p)
      p
    })
  }
            
}

trait TerminalParser extends MeerkatParser

class StartChar(c1: Char) {
  def --(c2: Char): TerminalParser
    = terminal((input, sppf, i) => if(i >= input.length) failure
                                   else {
                                     val c = input.charAt(i)
                                     if(c >= c1 && c <= c2) success(sppf.getTerminalNode(c, i)) 
                                     else failure 
                                   }, "[" + c1 + "-" + c2 + "]")
    
  def -!(c2: Char): TerminalParser
    = terminal((input, sppf, i) => if(i >= input.length) failure
                                   else {
                                     val c = input.charAt(i)
                                     if(c < c1 || c > c2) success(sppf.getTerminalNode(c, i)) 
                                     else failure
                                   }, "[" + c1 + "-" + c2 + "]")      
}
  
object MeerkatParser {
  
  import MeerkatDDParser._
  
  implicit def toDDParser(p: MeerkatParser): MeerkatDDParser[Unit] = {
    val q = ddparser((input, sppf, i) => p(input, sppf, i).mapNoMemo(t => (t, ())))
    q.nameAs(p.name.value)
    if(p.sequenced) q.sequence
    q.resetWith(p.reset())
    q
  }
  
}

import MeerkatDDParser._

trait MeerkatDDParser[T] extends DDParser[T] {
  
  var name: Lazy[String] = this.hashCode().toString
  def nameAs(name: => String): Unit = this.name = new Lazy(name)
  override def toString: String = this.name.value
    
  var head: String = ""
  def head(head: String): Unit = this.head = head
  def headed: Boolean = !head.isEmpty()
    
  var seq = false
  def sequenced: Boolean = seq
  def sequence: Unit = seq = true
    
  var reset: Unit => Unit = { _ => }
  def resetWith(f: => Unit): Unit = reset = { _ => f }
    
  def map[U](f: T => U): MeerkatDDParser[U] = {
    val p = ddparser((input, sppf, i) => this(input, sppf, i).map(t => (t._1, f(t._2))))
    p.nameAs(this.name.value)
    if(this.sequenced) p.sequence
    p.resetWith(this.reset())
    p
  }
      
  def ~~^[U](f: T => MeerkatDDParser[U]): MeerkatDDParser[U] = {
    lazy val p: MeerkatDDParser[U] 
      = ddparser((input, sppf, i) => this(input, sppf, i).flatMap(t1 => f(t1._2)(input, sppf, t1._1.rightExtent).mapNoMemo(t2 => 
                                            (if(p.headed) sppf.getNonterminalNode(p.head, p, Option(t1._1), t2._1) 
                                             else sppf.getIntermediateNode(p, t1._1, t2._1), 
                                             t2._2))))
    p.nameAs((if(p.headed) p.head + " ::= " else "") + this.name.value + f.toString)
    p.sequence
    p.resetWith(this.reset())
    p
  }
  
  def ~~>[U](q: MeerkatDDParser[U]): MeerkatDDParser[T] = {
    val p = this ~~^ (t => q.map(_ => t))
    p.nameAs((if(p.headed) p.head + " ::= " else "") + this.name.value + q.name.value)
    p.resetWith({ this.reset(); q.reset() })
    p
  }
  
    def ~~^[U](q: MeerkatDDParser[U]): MeerkatDDParser[U] = {
    val p = this ~~^ (t => q)
    p.nameAs((if(p.headed) p.head + " ::= " else "") + this.name.value + q.name.value)
    p.resetWith({ this.reset(); q.reset() })
    p
  }
    
  def ~^[U](f: T => MeerkatDDParser[U])(implicit l: Layout): MeerkatDDParser[U] = this ~~> l.parser ~~^ f    
  def ~>[U](q: MeerkatDDParser[U])(implicit l: Layout): MeerkatDDParser[T] = this ~~> l.parser ~~> q
  def ~^[U](q: MeerkatDDParser[U])(implicit l: Layout): MeerkatDDParser[U] = this ~~> l.parser ~~^ q
        
  def toStr: MeerkatDDParser[String] = {
    val p = ddparser((input, sppf, i) => this(input, sppf, i).mapNoMemo(t => (t._1, input.substring(t._1.leftExtent, t._1.rightExtent))))
    p.nameAs(this.name.value)
    if(this.sequenced) p.sequence
    p.resetWith(this.reset())
    p
  }
    
  def addColumn: MeerkatDDParser[Int] = {
    val p = ddparser((input, sppf, i) => this(input, sppf, i).mapNoMemo(t => (t._1, input.columnNumber(i))))
    p.nameAs(this.name.value)
    if(this.sequenced) p.sequence
    p.resetWith(this.reset())
    p
  }
    
}

object MeerkatDDParser {
  
  implicit def toParser[T](p: MeerkatDDParser[T]): MeerkatParser = {
    lazy val q: MeerkatParser 
      = new MeerkatParser {
          lazy val _p = { if(q.headed) p.head(q.head); p }
          def apply(input: Input, sppf: SPPFLookup, i: Int) = _p(input, sppf, i).mapNoMemo(t => t._1) 
        }
    q.nameAs(p.name.value)
    if(p.sequenced) q.sequence
    q.resetWith(p.reset())
    q
  }
  
  def ddparser[T](f: DDParser[T])
    = new MeerkatDDParser[T] { def apply(input: Input, sppf: SPPFLookup, i: Int) = f(input, sppf, i) }
}

import Configuration._

trait MeerkatParsers {
    
  val Layout: MeerkatParser = "L" ::= util.JavaTokens.Layout
  
  implicit val L = new Layout {
    def parser: MeerkatParser = Layout
  }
  
  
  private def run(input: Input, sppf: SPPFLookup, parser: MeerkatParser, parseOpt: Configuration): Unit = {
    parser(input, sppf, 0)(t => if(t.rightExtent == input.length) { 
                                  MeerkatLogging.logger.info("FIRST SUCCESSFUL PARSE : " + t) 
                                  if(parseOpt == FIRST_PARSE) return
                                })
    Trampoline.run
  }
  
  def parse(input: String, parser: MeerkatParser, parseOpt: Configuration, testOpt: Configuration): ParseResult = {
    
    MeerkatLogging.reset
    if(testOpt == TESTING) MeerkatLogging.logger.setLevel(INFO)
    else MeerkatLogging.logger.setLevel(DEBUG)
    MeerkatLogging.logger.info("Input size: %d", new Integer(input.size))
    
    Layout.reset()
    parser.reset()
    
    val startUserTime: Long = getUserTime
    val startNanoTime: Long = System.nanoTime()
    
    val sppf = new DefaultSPPFLookup()    
    run(new Input(input), sppf, parser, parseOpt)
    
    val startSymbol = sppf.getStartNode(parser.name.value, 0, input.length())
    
    val endUserTime: Long = getUserTime
    val endNanoTime: Long = System.nanoTime()
    
    MeerkatLogging.logger.info("Parsing time (user): %d ms", new java.lang.Long((endUserTime - startUserTime) / 1000000))
    MeerkatLogging.logger.info("Parsing time (nano): %d ms", new java.lang.Long((endNanoTime - startNanoTime) / 1000000))
    
    startSymbol match {
        case None    => println("Parse error")
        case Some(node) => println("Success: " + node)
        				println(sppf.countAmbiguousNodes + ", " + sppf.countIntermediateNodes + ", " + sppf.countPackedNodes + ", " + sppf.countNonterminalNodes + ", " + sppf.countTerminalNodes)
                        if(testOpt == NO_TESTING) {
                          println("Visualizing...") 
                          Visualization.toDot(startSymbol.get)
                          println("Done!")
                        }
    }
    
    startSymbol match {
	  case None    => ParseError(0, " ")
	  case Some(_) => ParseSuccess((endNanoTime - startNanoTime) / 1000000, 
			  					   (endUserTime - startUserTime) / 1000000,
			  					   (1000000) / 1000000,
			  					   startSymbol.get,
			  					   sppf.countNonterminalNodes,
			  					   sppf.countIntermediateNodes,
			  					   sppf.countTerminalNodes,
			  					   sppf.countPackedNodes,
			  					   sppf.countAmbiguousNodes) 
    }
  }
  
 def parse(input: String, parser: MeerkatParser, parseOpt: Configuration): ParseResult = {
	
	MeerkatLogging.reset
	MeerkatLogging.logger.setLevel(SEVERE)
    Layout.reset()
    parser.reset()
    
	val startUserTime = getUserTime
	val startSystemTime = getCpuTime
	val startNanoTime = System.nanoTime
	
	val sppf = new DefaultSPPFLookup()
	run(new Input(input), sppf, parser, parseOpt)    
	val startSymbol = sppf.getStartNode(parser.name.value, 0, input.length())
	    
	val endUserTime: Long = getUserTime
	val endSystemTime = getCpuTime
	val endNanoTime: Long = System.nanoTime
	
	startSymbol match {
	  case None    => ParseError(0, " ")
	  case Some(_) => ParseSuccess((endNanoTime - startNanoTime) / 1000000, 
			  					   (endUserTime - startUserTime) / 1000000,
			  					   (endSystemTime - startSystemTime) / 1000000,
			  					   startSymbol.get,
			  					   sppf.countNonterminalNodes,
			  					   sppf.countIntermediateNodes,
			  					   sppf.countTerminalNodes,
			  					   sppf.countPackedNodes,
			  					   sppf.countAmbiguousNodes) 
    }
  }
      
}
