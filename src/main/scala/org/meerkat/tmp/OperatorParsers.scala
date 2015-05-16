package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import java.util.HashMap
import org.meerkat.sppf.DefaultSPPFLookup
import org.meerkat.util.Visualization

object OperatorParsers {
  
  import AbstractOperatorParsers._
  
  trait OperatorSequence extends AbstractOperatorSequence[NonPackedNode]
  
  trait OperatorParserWithAlternationOp extends AbstractOperatorParser[NonPackedNode] {
    def | (p: OperatorSequence): OperatorAlternation = ???   
    def | (p: Nonterminal): OperatorAlternation = ???

    def | (p: Parsers.Sequence): OperatorAlternation = ???
    def | (p: Parsers.Symbol): OperatorAlternation = ???
    
    def |> (p: Sequence): OperatorAlternation = ???
    def |> (p: Nonterminal): OperatorAlternation = ???
  }
  
  trait OperatorAlternation extends AbstractOperatorParser[NonPackedNode]
  
  trait OperatorNonterminal extends AbstractOperatorParser[NonPackedNode]
  
  trait HasAlternationOp extends AbstractOperatorParser[NonPackedNode] {
    def | (p: Sequence): Alternation = alternation(this, p)   
    def | (p: Nonterminal): Alternation = alternation(this, p)

    def | (p: Parsers.Sequence): Alternation = alternation(this, p)
    def | (p: Parsers.Symbol): Alternation = alternation(this, p)
    
    def |> (p: Sequence): Alternation = greater(this, p)
    def |> (p: Nonterminal): Alternation = greater(this, p)
    
  }
  
  trait Sequence extends HasAlternationOp { 
    override def isSequence = true
    
    private var lvl: Int = -1
    override def level: Int = lvl
    
    private var group: Group = _
    override def pass(group: Group) = {
      if (isLeftRec || isRightRec) {
        this.group = group
        this.lvl =  this.group.get(this.assoc)
      }
    }
    
    protected def condition: Prec => Boolean = {
      if (isLeftRec && isRightRec) {
        if (group.canClimb(level)) {
          return prec => group.maximum >= prec._1 && group.maximum >= prec._2  
        } else {
          assoc match {
            case Assoc.UNDEFINED => return prec => group.maximum >= prec._1 && group.maximum >= prec._2
            case Assoc.LEFT      => return prec => group.maximum >= prec._1 && group.maximum >= prec._2 && level != prec._2
            case Assoc.RIGHT     => return prec => group.maximum >= prec._1 && group.maximum >= prec._2 && level != prec._1
            case _ =>
          }
        }
      } else if (isLeftRec) {
        if (group.canClimb(level)) {
          return prec => group.maximum >= prec._2  
        } else {
          assoc match {
            case Assoc.UNDEFINED => return prec => group.maximum >= prec._2
            case Assoc.LEFT      => return prec => group.maximum >= prec._2 && level != prec._2
            case _ =>
          }
        }
      } else if (isRightRec) {
        if (group.canClimb(level)) {
          return prec => group.maximum >= prec._1  
        } else {
          assoc match {
            case Assoc.UNDEFINED => return prec => group.maximum >= prec._1
            case Assoc.RIGHT     => return prec => group.maximum >= prec._1 && level != prec._1
            case _ =>
          }
        }
      }
      
      return null
    }
    
    protected def arguments: (Prec, Prec) = {
      if (isLeftRec || isRightRec) {
        if (group.canClimb(level)) {
          assoc match {
            case Assoc.UNDEFINED => 
              return (if (isLeftRec) (level, level) else $, 
                      if (isRightRec) (level, level) else $)
            case Assoc.LEFT => 
              return (if (isLeftRec) (level, level) else $, if (isRightRec) (level + 1, level + 1) else $)
            case Assoc.RIGHT => 
              return (if (isLeftRec) (level + 1, level + 1) else $, if (isRightRec) (level, level) else $)
          }
        } else {
          return (if (isLeftRec) (level, level) else $, if (isRightRec) (level, level) else $)
        }
      }
      return ($,$)
    }
    
  }
  
  trait Alternation extends HasAlternationOp { override def isAlternation = true }
  
  def alternation(p1: AbstractOperatorParser[NonPackedNode], p2: AbstractOperatorParser[NonPackedNode]) 
    = new Alternation { import Parsers._
        def apply(prec: Prec): Parsers.Alternation = AbstractCPSParsers.AbstractParser.alt(p1(prec), p2(prec))
        
        override def pass(head: AbstractOperatorParser[Any]) = { p1 pass head; p2 pass head }
        override def pass(group: Group) = { p2 pass group; p1 pass group; if (p1.isSequence) group.finalise }
      }
  
  def alternation(p1: AbstractOperatorParser[NonPackedNode], p2: AbstractCPSParsers.AbstractParser[NonPackedNode]) 
    = new Alternation { import Parsers._
        def apply(prec: Prec): Parsers.Alternation = AbstractCPSParsers.AbstractParser.alt(p1(prec), p2)
        
        override def pass(head: AbstractOperatorParser[Any]) = { p1 pass head }
        override def pass(group: Group) = { p1 pass group; if (p1.isSequence) group.finalise }
      }
  
  def alternation(p1: AbstractCPSParsers.AbstractParser[NonPackedNode], p2: AbstractOperatorParser[NonPackedNode]) 
    = new Alternation { import Parsers._
        def apply(prec: Prec): Parsers.Alternation = AbstractCPSParsers.AbstractParser.alt(p1, p2(prec))
        override def pass(head: AbstractOperatorParser[Any]) = { p2 pass head }
        override def pass(group: Group) = { p2 pass group; if (p2.isSequence) group.finalise }
      }
  
  def greater(p1: AbstractOperatorParser[NonPackedNode], p2: AbstractOperatorParser[NonPackedNode]) 
    = new Alternation { import Parsers._
        def apply(prec: Prec): Parsers.Alternation = AbstractCPSParsers.AbstractParser.alt(p1(prec), p2(prec))
        
        override def pass(head: AbstractOperatorParser[Any]) = { p1 pass head; p2 pass head }
    
        override def pass(group: Group) = {
          p2 pass group
          
          val newGroup = group.startNew(Assoc.UNDEFINED)
          p1 pass newGroup
          if (p1.isSequence) newGroup.finalise
        }
      }
    
  trait Nonterminal extends HasAlternationOp {
    override def isNonterminal = true
    
    override def pass(head: AbstractOperatorParser[Any]) = {}
    override def pass(group: Group) = {}
    
    def ~ (p: Parsers.Symbol): Postfix = Postfix(this, p)
    def ~ (p: Nonterminal): Infix = Infix(this, p)
  }
  
  object FailingSequence extends Parsers.Sequence {
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = CPSResult.failure[NonPackedNode]
  }
  
  case class Postfix(p1: AbstractOperatorParser[NonPackedNode], p2: AbstractCPSParsers.AbstractParser[NonPackedNode]) extends Sequence { import Parsers._
    
    lazy val cond = condition
    lazy val args = arguments
    
    def apply(prec: Prec): Parsers.Sequence = {
      if (level == -1) 
        AbstractCPSParsers.AbstractParser.seq(p1(prec), p2)
      else if (cond(prec))
        AbstractCPSParsers.AbstractParser.seq(p1(args._1), p2)
      else
        FailingSequence
    }
    
    protected var rec: Rec.Rec = Rec.UNDEFINED
    override def isLeftRec = rec == Rec.LEFT
    
    override def pass(head: AbstractOperatorParser[Any]) = {
      val isLeftRec = if (p1 isNonterminal) p1 == head else { p1 pass head; p1 isLeftRec }
      if (isLeftRec) rec = Rec.LEFT
    }
  
    def ~ (p: Parsers.Symbol): Postfix = Postfix(this, p)
    def ~ (p: OperatorParsers.Nonterminal): Infix = Infix(this, p)
  }
  
  case class Prefix(p1: AbstractCPSParsers.AbstractParser[NonPackedNode], p2: Nonterminal) extends Sequence { import Parsers._
    
    lazy val cond = condition
    lazy val args = arguments
    
    def apply(prec: Prec): Parsers.Sequence = {
      if (level == -1)
        AbstractCPSParsers.AbstractParser.seq(p1, p2(prec))
      else if (cond(prec))
        AbstractCPSParsers.AbstractParser.seq(p1, p2(args._2))
      else 
        FailingSequence
    }
    
    protected var rec: Rec.Rec = Rec.UNDEFINED
    override def isRightRec = rec == Rec.RIGHT
    
    override def pass(head: AbstractOperatorParser[Any]) = if (p2 == head) rec = Rec.RIGHT
        
    def ~ (p: Parsers.Symbol): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(Prefix.this($), p)
    def ~ (p: OperatorParsers.Nonterminal): Prefix = Prefix(Prefix.this($), p)
  }
  
  case class Infix(p1: AbstractOperatorParser[NonPackedNode], p2: Nonterminal) extends Sequence { import Parsers._
    
    lazy val cond = condition
    lazy val args = arguments
    
    def apply(prec: Prec): Parsers.Sequence = {
      if (level == -1)
        AbstractCPSParsers.AbstractParser.seq(p1(prec), p2(prec))
      else if (cond(prec))
        AbstractCPSParsers.AbstractParser.seq(p1(args._1), p2(args._2))
      else 
        FailingSequence
    }
    
    protected var rec: Rec.Rec = Rec.UNDEFINED
    
    override def isLeftRec = rec == Rec.LEFT || rec == Rec.BOTH
    override def isRightRec = rec == Rec.RIGHT || rec == Rec.BOTH
    
    override def pass(head: AbstractOperatorParser[Any]) = {
      val isLeftRec = if (p1 isNonterminal) p1 == head else { p1 pass head; p1 isLeftRec }
      if (isLeftRec) {
        if (p2 == head) rec = Rec.BOTH else rec = Rec.LEFT
      } else {
        if (p2 == head) rec = Rec.RIGHT
      }
    }
    
    def ~ (p: Parsers.Symbol): Postfix = Postfix(Postfix(p1, p2($)), p)
    def ~ (p: OperatorParsers.Nonterminal): Infix = Infix(Postfix(p1, p2($)), p)
  }
   
  def left(p: Infix): Infix = new Infix(p.p1, p.p2) { override def assoc = Assoc.LEFT }
  def right(p: Infix): Infix = new Infix(p.p1, p.p2) { override def assoc = Assoc.RIGHT }
  
  def left(p: Alternation): Alternation = ???
  def right(p: Alternation): Alternation = ???
  
  implicit class ParsersOp(p: AbstractCPSParsers.AbstractParser[NonPackedNode]) {
    def ~ (q: OperatorParsers.Nonterminal): Prefix = Prefix(p, q)
    
    def | (q: Sequence): Alternation = alternation(p, q)
    
    def | (q: Nonterminal): Alternation = alternation(p, q)
  }
  
  def op_nt(name: String)(p: => AbstractOperatorParser[NonPackedNode]): Nonterminal
    = new Nonterminal {
        import Parsers._
        val table: java.util.Map[Prec, Parsers.Nonterminal] = new HashMap()
        
        lazy val q = { 
          val _p = p
          _p pass this; _p pass Group()
          _p 
        }
        
        def apply(prec: Prec) 
          = if (table.containsKey(prec)) table.get(prec) 
            else { 
              val nt = AbstractCPSParsers.memoize(q(prec), name + s"$prec")
              table.put(prec, nt)
              nt
            }
        
      }
  
  def run(input: Input, sppf: SPPFLookup, parser: AbstractCPSParsers.AbstractParser[NonPackedNode]): Unit = {
    parser(input, 0, sppf)(t => if(t.rightExtent == input.length) { println(s"Success: $t")  })
    Trampoline.run
  }
  
  def parse(sentence: String, parser: Nonterminal): Unit = {
    val input = new Input(sentence)
    val sppf = new DefaultSPPFLookup(input)
    
    val p = parser((0,0))
    run(input, sppf, p)
    
    println(s"Trying to find: ${p.name}(0,${sentence.length()})")
    val startSymbol = sppf.getStartNode(p, 0, sentence.length())
    
    startSymbol match {
      case None       => println("Parse error")
      case Some(node) => println("Success: " + node)
                         println(sppf.countAmbiguousNodes + ", " + sppf.countIntermediateNodes + ", " + sppf.countPackedNodes + ", " + sppf.countNonterminalNodes + ", " + sppf.countTerminalNodes)
                         println("Visualizing...") 
                         Visualization.visualize(Visualization.toDot(startSymbol.get), "sppf")
                         println("Done!")
    }
  }
  
}