package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import java.util.HashMap

object OperatorParsers {
  
  import AbstractOperatorParsers._
  
  trait HasAlternationOp extends AbstractOperatorParser[NonPackedNode] {
    def | (p: Sequence): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) })
        .set(this or p)
    
    def | (p: Nonterminal): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) })
        .set(this or p)

    def | (p: Parsers.Sequence): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p) })
        .set(this.asGroups)
    
    def | (p: Parsers.Symbol): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p) })
        .set(this.asGroups)
    
    def |> (p: Sequence): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) })
        .set(this greater p)
    
    def |> (p: Nonterminal): Alternation 
      = alternation({ import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(this(prec), p(prec)) })
        .set(this greater p)
    
  }
  
  trait Sequence extends HasAlternationOp {
    override def isSequence = true
  }
  
  trait Alternation extends HasAlternationOp {
    override def isAlternation = true
    
    private var recursives: Groups[NonPackedNode] = _
    
    override def asGroups: Groups[NonPackedNode] = recursives
      
    def set(recursives: Groups[NonPackedNode]): Alternation
      = { this.recursives = recursives; this }
    
  }
  
  def alternation(f: Prec => Parsers.Alternation): Alternation
    = new Alternation { 
        def apply(prec: Prec): Parsers.Alternation = f(prec)
      }
  
  trait Nonterminal extends HasAlternationOp {
    override def isNonterminal = true
    
    def ~ (p: Parsers.Symbol): Postfix = Postfix(this, p)
    def ~ (p: Nonterminal): Infix = Infix(this, p)
  }
  
  case class Postfix(p1: AbstractOperatorParser[NonPackedNode], p2: AbstractCPSParsers.AbstractParser[NonPackedNode]) extends Sequence { import Parsers._
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1(prec), p2)
    
    override def isLeft[U](p: AbstractOperatorParser[U]) = if (p1 isNonterminal) (p1 == p) else p1.isLeft(p)
    
    def ~ (p: Parsers.Symbol): Postfix = Postfix(this, p)
    def ~ (p: OperatorParsers.Nonterminal): Infix = Infix(this, p)
  }
  
  case class Prefix(p1: AbstractCPSParsers.AbstractParser[NonPackedNode], p2: Nonterminal) extends Sequence { import Parsers._
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1, p2(prec))
    
    override def isRight[U](p: AbstractOperatorParser[U]) = (p2 == p)
    
    def ~ (p: Parsers.Symbol): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(Prefix.this($), p)
    def ~ (p: OperatorParsers.Nonterminal): Prefix = Prefix(Prefix.this($), p)
  }
  
  case class Infix(p1: AbstractOperatorParser[NonPackedNode], p2: Nonterminal) extends Sequence { import Parsers._
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1(prec), p2(prec))
    
    override def isLeft[U](p: AbstractOperatorParser[U]) = if (p1 isNonterminal) (p1 == p) else p1.isLeft(p)
    override def isRight[U](p: AbstractOperatorParser[U]) = (p2 == p)
    
    def ~ (p: Parsers.Symbol): Postfix = Postfix(Postfix(p1, p2($)), p)
    def ~ (p: OperatorParsers.Nonterminal): Infix = Infix(Postfix(p1, p2($)), p)
  }
  
  case class LeftInfix(p1: AbstractOperatorParser[NonPackedNode], p2: Nonterminal) extends Sequence { import Parsers._
    override def isLeft = true
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1(prec), p2(prec))
    
    override def isLeft[U](p: AbstractOperatorParser[U]) = if (p1 isNonterminal) (p1 == p) else p1.isLeft(p)
    override def isRight[U](p: AbstractOperatorParser[U]) = (p2 == p)
  }
  
  case class RightInfix(p1: AbstractOperatorParser[NonPackedNode], p2: Nonterminal) extends Sequence { import Parsers._
    override def isRight = true
    
    def apply(prec: Prec): Parsers.Sequence = AbstractCPSParsers.AbstractParser.seq(p1(prec), p2(prec))
    
    override def isLeft[U](p: AbstractOperatorParser[U]) = if (p1 isNonterminal) (p1 == p) else p1.isLeft(p)
    override def isRight[U](p: AbstractOperatorParser[U]) = (p2 == p)
  }
  
  def left(p: Infix): LeftInfix = LeftInfix(p.p1, p.p2)
  def right(p: Infix): RightInfix = RightInfix(p.p1, p.p2)
  
  def left(p: Alternation): Alternation = ???
  def right(p: Alternation): Alternation = ???
  
  implicit class ParsersOp(p: AbstractCPSParsers.AbstractParser[NonPackedNode]) {
    def ~ (q: OperatorParsers.Nonterminal): Prefix = Prefix(p, q)
    
    def | (q: Sequence): Alternation 
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(p, q(prec)) }
    
    def | (q: Nonterminal): Alternation
      = alternation { import Parsers._; prec => AbstractCPSParsers.AbstractParser.alt(p, q(prec)) }
  }
  
  def op_nt(name: String)(p: => AbstractOperatorParser[NonPackedNode]): Nonterminal
    = new Nonterminal {
        import Parsers._
        val table: java.util.Map[Prec, Parsers.Nonterminal] = new HashMap()
        
        lazy val groups: Groups[NonPackedNode] = p.asGroups 
        
        def apply(prec: Prec) 
          = if (table.containsKey(prec)) table.get(prec) 
            else { 
              val nt = AbstractCPSParsers.memoize(p(prec), name + s"$prec")
              table.put(prec, nt)
              nt
            }
        
        override def precedence = new Precedence
        
      }
  
  private def assignPrecedence(groups: Groups[NonPackedNode]): Unit = {
    val recsBackwards = groups.recs.reverse
    var atBackwards: List[Int] = List()
    
    var i = 0
    for (_ <- groups.at)
      if (i <= groups.at.length) {
        val next = groups.at(i + 1)
        i += 1
        atBackwards = atBackwards.::(groups.at.length - (next - 1)) // next - 1 is the last element in the current group
      } else {
        atBackwards = atBackwards.::(0)  
      }
    
    val leftAtBackwards = groups.leftAt map { x => groups.recs.length - x - 1 }
    val rightAtBackwards = groups.rightAt map { x => groups.recs.length - x - 1 }
    
    val precedence = new Precedence
    
    i = 0
    var group = precedence.newGroup(Assoc.UNDEFINED)
    for (rec <- recsBackwards) {
      if (i != 0 && atBackwards.contains(i))
        group = precedence.newGroup(Assoc.UNDEFINED)
      
      if (rec.isLeft || rec.isRight)
        precedence.incr
      
      rec assign { precedence.counter }
      rec assign { group }
      
      i += 1
    }
    
  } 

}