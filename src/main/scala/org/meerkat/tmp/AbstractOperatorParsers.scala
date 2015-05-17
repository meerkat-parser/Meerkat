package org.meerkat.tmp

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input

object Rec extends Enumeration {
  type Rec = Value
  val UNDEFINED, LEFT, RIGHT, BOTH = Value
}

object Assoc extends Enumeration {
  type Assoc = Value
  val UNDEFINED, LEFT, RIGHT, ASSOC, NON_ASSOC = Value
}

object AbstractOperatorParsers {
  
  import AbstractCPSParsers._
  
  type Prec = (Int, Int)
  
  val $: Prec = (0,0)
  
  trait AbstractOperatorParser[+T] extends (Prec => AbstractParser[T]) {
    
    protected def head: Option[AbstractOperatorParser[Any]] = None
    protected def group: Option[Group] = None
     
    def pass(head: AbstractOperatorParser[Any], group: Group): Unit = {}
    
    def isSequence = false
    def isAlternation = false
    def isNonterminal = false
  }
  
  trait AbstractOperatorSequence[+T] extends ((Prec, Prec) => AbstractParser[T]) {
    
    def assoc: Assoc.Assoc = Assoc.UNDEFINED
    
    protected def head: Option[AbstractOperatorParser[Any]] = None
    def pass(head: AbstractOperatorParser[Any]): Unit = {}
    
    def isSequence = true
    
    def isInfix = false
    def isPostfix = false
    def isPrefix = false
  }
  
  trait CanBuildSequence[A, B] {
    implicit val obj: AbstractCPSParsers.CanBuildSequence[A, B]
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]
    
    type OperatorSequence <: AbstractOperatorSequence[obj.R]
    def sequence(f: AbstractOperatorParser[Any] => OperatorSequence): OperatorSequence
    
    def infixOp(f: (Prec, Prec) => obj.Sequence): OperatorSequence
    def postfixOp(f: (Prec, Prec) => obj.Sequence): OperatorSequence
    def prefixOp(f: (Prec, Prec) => obj.Sequence): OperatorSequence
    def sequenceOp(f: (Prec, Prec) => obj.Sequence): OperatorSequence
    
    def left(f: OperatorSequence): OperatorSequence
    def right(f: OperatorSequence): OperatorSequence
    def non_assoc(f: OperatorSequence): OperatorSequence
  }
  
  trait CanBuildAlternation[A, B >: A] {
    implicit val obj1: AbstractCPSParsers.CanBuildAlternation[A]
    implicit val obj2: AbstractCPSParsers.CanBuildAlternation[B]
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]
    
    type OperatorAlternation <: AbstractOperatorParser[B]
    def alternation(f: (AbstractOperatorParser[Any], Group) => Prec => AbstractParser[B]): OperatorAlternation
  }
  
  object AbstractOperatorParser {
    /**
     * @param p1 shouldn't be a result of alternation
     * @param p2 shouldn't be a result of sequence or alternation
     */
    def seq[A, B](p1: AbstractOperatorParser[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildSequence[A,B]): builder.OperatorSequence = {
      import builder._
      sequence { head =>
        val isLeftRec = p1 == head; val isRightRec = p2 == head
        val q: (Prec, Prec) => obj.Sequence = (prec1, prec2) => AbstractParser.seq(p1(prec1), p2(prec2)) 
        if (isLeftRec && isRightRec) infixOp(q)
        else if (isLeftRec) postfixOp(q)
        else if (isRightRec) prefixOp(q)
        else sequenceOp(q)
      }
    }
    def seq[A, B](p1: AbstractParser[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildSequence[A,B]): builder.OperatorSequence = {
      import builder._
      sequence { head =>
        val q: (Prec, Prec) => obj.Sequence = (prec1, prec2) => AbstractParser.seq(p1, p2(prec2))
        if (p2 == head) prefixOp(q) else sequenceOp(q)
      }
    }
    def seq[A, B](p1: AbstractOperatorParser[A], p2: AbstractParser[B])(implicit builder: CanBuildSequence[A,B]): builder.OperatorSequence = {
      import builder._
      sequence { head =>
        val q: (Prec, Prec) => obj.Sequence = (prec1, prec2) => AbstractParser.seq(p1(prec1), p2)
        if (p1 == head) postfixOp(q) else sequenceOp(q)
      }
    }
    def seq[A, B](p1: AbstractOperatorSequence[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildSequence[A,B]): builder.OperatorSequence = {
      import builder._
      sequence { head =>
        p1 pass head
        val isLeftRec = p1.isInfix || p1.isPostfix; val isRightRec = p2 == head
        val q: (Prec, Prec) => obj.Sequence = (prec1, prec2) => AbstractParser.seq(p1(prec1, $), p2(prec2))
        if (isLeftRec && isRightRec) infixOp(q)
        else if (isLeftRec) postfixOp(q)
        else if (isRightRec) prefixOp(q)
        else sequenceOp(q)
      }
    }
    def seq[A, B](p1: AbstractOperatorSequence[A], p2: AbstractParser[B])(implicit builder: CanBuildSequence[A,B]): builder.OperatorSequence = {
      import builder._
      sequence { head =>
        p1 pass head
        val q: (Prec, Prec) => obj.Sequence = (prec1, prec2) => AbstractParser.seq(p1(prec1, $), p2)
        if (p1.isInfix || p1.isPostfix) postfixOp(q)
        else sequenceOp(q)
      }
    }
    
    def left[A](implicit builder: CanBuildSequence[A,A]): builder.OperatorSequence => builder.OperatorSequence 
      = { p => builder.sequence { head => p pass head; if (p.isInfix) builder.left(p) else p } }
    
    def right[A](implicit builder: CanBuildSequence[A,A]): builder.OperatorSequence => builder.OperatorSequence 
      = { p => builder.sequence { head => p pass head; if (p.isInfix) builder.right(p) else p } }
    
    def non_assoc[A](implicit builder: CanBuildSequence[A,A]): builder.OperatorSequence => builder.OperatorSequence 
      = { p => builder.sequence { head => p pass head; if (p.isInfix || p.isPrefix || p.isPostfix) builder.non_assoc(p) else p } }
    
    def greater[A, B >: A](p1: AbstractOperatorParser[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => 
        p2 pass (head, group); p1 pass (head, if (!p2.isNonterminal) group.startGroup else group)
        prec => AbstractParser.alt(p1(prec), p2(prec))
      }
    }
    
    def greater[A, B >: A](p1: AbstractOperatorParser[A], p2: AbstractOperatorSequence[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => 
        p2 pass head; val l = level(p2, group); 
        val subgroup = group.subgroup 
        p1 pass (head, group.startGroup); println(s"|>: ($l, $group); subgroup: $subgroup")
        if (l != -1) {
          val q2 = filter(p2, l, group, subgroup)
          prec => AbstractParser.alt(p1(prec), q2(prec))
        } else prec => AbstractParser.alt(p1(prec), p2(prec, prec))
      }
    }
    
    def greater[A, B >: A](p1: AbstractOperatorSequence[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => 
        p2 pass (head, group); p1 pass head
        val next = group.startGroup; val l = level(p1, next);  
        val subgroup = group.subgroup; next.closeGroup
        println(s"|>: ($l, $group); subgroup: $subgroup")
        if (l != -1) {
          val q1 = filter(p1, l, group, subgroup)
          prec => AbstractParser.alt(q1(prec), p2(prec))
        } else prec => AbstractParser.alt(p1(prec, prec), p2(prec))
      }
    }
    
    def greater[A, B >: A](p1: AbstractOperatorSequence[A], p2: AbstractOperatorSequence[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => 
        p2 pass head; val l2 = level(p2, group)
        p1 pass head; val next = group.startGroup; val l1 = level(p1, next); next.closeGroup
        val subgroup = group.subgroup // By construction, either None or the same
        println(s"|>: ($l1, $next), ($l2, $group); subgroup: $subgroup")
        if (l1 != -1 && l2 != -1) {
          val q1 = filter(p1, l1, next, subgroup); val q2 = filter(p2, l2, group, subgroup)
          prec => AbstractParser.alt(q1(prec), q2(prec))
        } else if (l1 != -1) {
          val q1 = filter(p1, l1, next, subgroup); prec => AbstractParser.alt(q1(prec), p2(prec, prec))
        } else if (l2 != -1) {
          val q2 = filter(p2, l2, group, subgroup); prec => AbstractParser.alt(p1(prec, prec), q2(prec))
        } else prec => AbstractParser.alt(p1(prec, prec), p2(prec, prec))
      }
    }
    
    /**
     * @param p2 can be a result of alternation 
     */
    def alt[A, B >: A](p1: AbstractOperatorParser[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => p2 pass (head, group); p1 pass (head, group)
        prec => AbstractParser.alt(p1(prec), p2(prec))
      }
    }
    
    def alt[A, B >: A](p1: AbstractOperatorParser[A], p2: AbstractParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => p1 pass (head, group)
        prec => AbstractParser.alt(p1(prec), p2)
      }
    }
    def alt[A, B >: A](p1: AbstractParser[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => p2 pass (head, group); group.closeGroup
        prec => AbstractParser.alt(p1, p2(prec))
      }
    }
    
    def alt[A, B >: A](p1: AbstractOperatorSequence[A], p2: AbstractOperatorSequence[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => 
        p1 pass head; p2 pass head
        val l2 = level(p2, group); val l1 = level(p1, group); group.closeGroup
        val subgroup = group.subgroup // By construction, either None or the same
        println(s"|: ($l1, $group), ($l2, $group); subgroup: $subgroup")
        if (l1 != -1 && l2 != -1) {
          val q1 = filter(p1, l1, group, subgroup); val q2 = filter(p2, l2, group, subgroup)
          prec => AbstractParser.alt(q1(prec), q2(prec))
        } else if (l1 != -1) {
          val q1 = filter(p1, l1, group, subgroup); prec => AbstractParser.alt(q1(prec), p2(prec, prec))
        } else if (l2 != -1) {
          val q2 = filter(p2, l2, group, subgroup); prec => AbstractParser.alt(p1(prec, prec), q2(prec))
        } else prec => AbstractParser.alt(p1(prec, prec), p2(prec, prec))
      }
    }
    
    def alt[A, B >: A](p1: AbstractOperatorSequence[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => 
        p2 pass (head, group); p1 pass head
        val l = level(p1, group); val subgroup = group.subgroup; group.closeGroup
        println(s"|: ($l, $group); subgroup: $subgroup")
        if (l != -1) {
          val q1 = filter(p1, l, group, subgroup); prec => AbstractParser.alt(q1(prec), p2(prec))
        } else prec => AbstractParser.alt(p1(prec, prec), p2(prec))
      }
    }
    def alt[A, B >: A](p1: AbstractOperatorParser[A], p2: AbstractOperatorSequence[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => 
        p2 pass head; val l = level(p2, group); val subgroup = group.subgroup 
        p1 pass (head, group)
        println(s"|: ($l, $group); subgroup: $subgroup")
        if (l != -1) {
          val q2 = filter(p2, l, group, subgroup)
          prec => AbstractParser.alt(p1(prec), q2(prec))
        } else prec => AbstractParser.alt(p1(prec), p2(prec, prec))
      }
    }
    
    def alt[A, B >: A](p1: AbstractOperatorSequence[A], p2: AbstractParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => 
        p1 pass head; val l = level(p1, group); val subgroup = group.subgroup; group.closeGroup
        println(s"|: ($l, $group); subgroup: $subgroup")
        if (l != -1) {
          val q1 = filter(p1, l, group, subgroup)
          prec => AbstractParser.alt(q1(prec), p2)
        } else prec => AbstractParser.alt(p1(prec, prec), p2)
      }
    }
    def alt[A, B >: A](p1: AbstractParser[A], p2: AbstractOperatorSequence[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { (head, group) => 
        p2 pass head; val l = level(p2, group); val subgroup = group.subgroup; group.closeGroup
        println(s"|: ($l, $group); subgroup: $subgroup")
        if (l != -1) {
          val q2 = filter(p2, l, group, subgroup)
          prec => AbstractParser.alt(p1, q2(prec))
        } else prec => AbstractParser.alt(p1, p2(prec, prec))
      }
    }
    
    def left[A](implicit builder: CanBuildAlternation[A,A]): builder.OperatorAlternation => builder.OperatorAlternation 
      = { p => builder.alternation { (head, group) => 
            val started = group.startSubgroup(Assoc.LEFT); p pass (head, group); if (started) group.closeSubgroup
            prec => p(prec) } 
        }
    
    def level[A](p: AbstractOperatorSequence[A], group: Group): Int
      = if (p.isInfix || p.isPrefix || p.isPostfix) {
          if (p.isPrefix) group.prefix; if (p.isPostfix) group.postfix; 
          group.get(p.assoc) 
        } else -1
    
    def filter[A](p: AbstractOperatorSequence[A], l: Int, group: Group, subgroup: Option[Group]): Prec => AbstractParser[A] = {
      val cond: Prec => Boolean = if (p.isInfix)       prec => group.maximum >= prec._1 && group.maximum >= prec._2
                                  else if (p.isPrefix) prec => group.maximum >= prec._1
                                  else                 prec => group.maximum >= prec._2
      if (!group.canClimb(l))
        p.assoc match {
          case Assoc.UNDEFINED => 
            return if (group.hasPostfixBelow && group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l,prec._2), (prec._1,l)) else fail
                   else if (group.hasPostfixBelow)
                     prec => if (cond(prec)) p((l,prec._2), (l,l)) else fail
                   else if (group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l,l), (prec._1,l)) else fail
                   else prec => if (cond(prec)) p((l,l), (l,l)) else fail
          case Assoc.LEFT      =>
            return if (group.hasPostfixBelow && group.hasPrefixBelow)
                     prec => if (cond(prec) && prec._2 != l) p((l,prec._2), (prec._1,l)) else fail
                   else if (group.hasPostfixBelow)
                     prec => if (cond(prec) && prec._2 != l) p((l,prec._2), (l,l)) else fail
                   else if (group.hasPrefixBelow)
                     prec => if (cond(prec) && prec._2 != l) p((l,l), (prec._1,l)) else fail
                   else prec => if (cond(prec) && prec._2 != l) p((l,l), (l,l)) else fail
          case Assoc.RIGHT     => 
            return if (group.hasPostfixBelow && group.hasPrefixBelow)
                     prec => if (cond(prec) && prec._1 != l) p((l,prec._2), (prec._1,l)) else fail
                   else if (group.hasPostfixBelow)
                     prec => if (cond(prec) && prec._1 != l) p((l,prec._2), (l,l)) else fail
                   else if (group.hasPrefixBelow)
                     prec => if (cond(prec) && prec._1 != l) p((l,l), (prec._1,l)) else fail
                   else prec => if (cond(prec) && prec._1 != l) p((l,l), (l,l)) else fail
          case Assoc.NON_ASSOC => 
            return if (group.hasPostfixBelow && group.hasPrefixBelow)
                     prec => if (cond(prec) && prec._1 != l && prec._2 != l) p((l,prec._2), (prec._1,l)) else fail
                   else if (group.hasPostfixBelow)
                     prec => if (cond(prec) && prec._1 != l && prec._2 != l) p((l,prec._2), (l,l)) else fail
                   else if (group.hasPrefixBelow)
                     prec => if (cond(prec) && prec._1 != l && prec._2 != l) p((l,l), (prec._1,l)) else fail
                   else prec => if (cond(prec) && prec._1 != l && prec._2 != l) p((l,l), (l,l)) else fail
        }
      else if (subgroup == None)
        p.assoc match {
          case Assoc.UNDEFINED => 
            return if (group.hasPostfixBelow && group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l,prec._2), (prec._1,l)) else fail
                   else if (group.hasPostfixBelow)
                     prec => if (cond(prec)) p((l,prec._2), (l,l)) else fail
                   else if (group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l,l), (prec._1,l)) else fail
                   else prec => if (cond(prec)) p((l,l), (l,l)) else fail
          case Assoc.LEFT      =>
            return if (group.hasPostfixBelow && group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l,prec._2), (prec._1,l+1)) else fail
                   else if (group.hasPostfixBelow)
                     prec => if (cond(prec)) p((l,prec._2), (l+1,l+1)) else fail
                   else if (group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l,l), (prec._1,l+1)) else fail
                   else prec => if (cond(prec)) p((l,l), (l+1,l+1)) else fail
          case Assoc.RIGHT     => 
            return if (group.hasPostfixBelow && group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l+1,prec._2), (prec._1,l)) else fail
                   else if (group.hasPostfixBelow)
                     prec => if (cond(prec)) p((l+1,prec._2), (l,l)) else fail
                   else if (group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l+1,l+1), (prec._1,l)) else fail
                   else prec => if (cond(prec)) p((l+1,l+1), (l,l)) else fail
          case Assoc.NON_ASSOC => 
            return if (group.hasPostfixBelow && group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l+1,prec._2), (prec._1,l+1)) else fail
                   else if (group.hasPostfixBelow)
                     prec => if (cond(prec)) p((l+1,prec._2), (l+1,l+1)) else fail
                   else if (group.hasPrefixBelow)
                     prec => if (cond(prec)) p((l+1,l+1), (prec._1,l+1)) else fail
                   else prec => if (cond(prec)) p((l+1,l+1), (l+1,l+1)) else fail
        } 
      else ??? // TODO: handle this case
    }
    
    object fail extends AbstractParser[Nothing] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = CPSResult.failure }
    
  }
  
  class Group(val assoc: Assoc.Assoc, min: Int) {
    
    private var max: Int = min
    private var undef: Int = -1
    
    def minimum = min
    def maximum = max
    
    private var prefixHere, postfixHere = false
    private var prefixBelow, postfixBelow = false
    
    def prefix: Unit = prefixHere = true
    def postfix: Unit = postfixHere = true
    
    def hasPrefixBelow = prefixBelow
    def hasPostfixBelow = postfixBelow
    
    private var started = false
    private var subgroups: List[Group] = List()
    
    def startGroup = { 
      if (!started) { // Precedence inside subgroups is ignored
        this.closeGroup; val group = new Group(Assoc.UNDEFINED, max + 1)
        group.prefixBelow = prefixHere || prefixBelow
        group.postfixBelow = postfixHere || postfixBelow
        group
      } else 
        this
    }
    
    def closeGroup = {
      if (started) this.closeSubgroup
      if (max != min) max -= 1 // min == max indicates that the group has not been used
    }
    
    def startSubgroup(assoc: Assoc.Assoc): Boolean 
      = if (!started) { // Subgroups inside subgroups are ignored 
          started = true; subgroups = subgroups.+:(new Group(assoc, max))
          true 
        } else false
    
    def closeSubgroup = {
      val group = subgroups(0)
      if (group.minimum != group.max) { // min == max indicates that the group has not been used
        max = group.max
        group.max -= -1
      } else 
        subgroups = subgroups.tail
      started = false
    }
    
    def get(assoc: Assoc.Assoc): Int
      = if (started) {
          val group = subgroups(0)
          if (assoc == Assoc.UNDEFINED || assoc == group.assoc) 
            group.get(Assoc.UNDEFINED)
          else 
            group.get(assoc)
        } else if (assoc == Assoc.UNDEFINED && undef == -1) {
          undef = max; max += 1
          undef
        } else if (assoc == Assoc.UNDEFINED) {
          undef
        } else {
          val cur = max; max += 1
          cur
        }
    
    def canClimb(level: Int): Boolean = {
      if (subgroups.isEmpty) 
        min == max
      else if (subgroups.length == 1) {
        val group = subgroups(0)
        if (this.min == group.minimum && this.max == group.max 
            && group.undef == level) true
        else false
      } else 
        false
    }
    
    def subgroup = if (started) Option(subgroups(0)) else None
    
    private var assocs: List[Group] = _
    
    override def toString = s"Group($min,$max,$undef,$prefixBelow,$postfixBelow)" 
  }
  
  object Group { def apply() = new Group(Assoc.UNDEFINED, 1) }
  
}