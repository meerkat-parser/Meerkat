package org.meerkat.tmp

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import org.meerkat.sppf.Slot

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
  
  type AbstractOperatorParser[+T] = (Prec => AbstractParser[T])
  
  type AbstractOperatorSequence[+T,V] = ((Prec, Prec) => AbstractSequenceBuilder[T] { type Value = V }) {
    def infix: Boolean; def prefix: Boolean; def postfix: Boolean; def assoc: Assoc.Assoc
  }
  
  type AbstractOperatorAlternation[+T,V] = Prec => AbstractAlternationBuilder[T] { type Value = V }
  
  type AbstractOperatorSymbol[+T,V] = Prec => AbstractSymbol[T] { type Value = V }
  
  type AbstractOperatorNonterminal[+T,V] = (Prec => AbstractNonterminal[T] { type Value = V })
  
  type Head = (Prec => AbstractNonterminal[Any] { type Value })
  
  type AbstractOperatorSequenceBuilder[+T,V] = Head => AbstractOperatorSequence[T,V]
  
  type AbstractOperatorAlternationBuilder[+T,V] = (Head, Group) => (Group => AbstractOperatorAlternation[T,V], Group, Option[Group])
    
  trait CanBuildSequence[A,B,ValA,ValB] {
    implicit val o: AbstractCPSParsers.CanBuildSequence[A,B,ValA,ValB]
    
    type AbstractOperatorSequence = ((Prec, Prec) => o.SequenceBuilder) {
      def infix: Boolean; def prefix: Boolean; def postfix: Boolean; def assoc: Assoc.Assoc
    }    
    type OperatorSequence <: AbstractOperatorSequence
    def sequence(p: AbstractOperatorSequence): OperatorSequence
    
    type OperatorSequenceBuilder <: (Head => OperatorSequence)
    def builderSeq(f: Head => OperatorSequence): OperatorSequenceBuilder
  }
  
   trait CanBuildAlternation[A,B >: A,ValA,ValB] {
    implicit val o: AbstractCPSParsers.CanBuildAlternation[A,B,ValA,ValB]
    type OperatorAlternation <: Prec => o.AlternationBuilder
    def alternation(f: Prec => o.AlternationBuilder): OperatorAlternation
    
    type OperatorAlternationBuilder <: (Head, Group) => (Group => OperatorAlternation, Group, Option[Group])
    def builderAlt(f: (Head, Group) => (Group => OperatorAlternation, Group, Option[Group])): OperatorAlternationBuilder
  }
   
  trait CanBuildNonterminal[A,ValA] {
    implicit val o1: AbstractCPSParsers.CanBuildNonterminal[A,ValA]
    implicit val o2: AbstractCPSParsers.CanBuildAlternative[A]
    
    type OperatorNonterminal <: Prec => o1.Nonterminal
    def nonterminal(name: String, f: Prec => o1.Nonterminal): OperatorNonterminal
  }
  
  object AbstractOperatorParser {
    
    def seqNt[A,B,ValA,ValB](p1: AbstractOperatorNonterminal[A,ValA], p2: AbstractOperatorNonterminal[B,ValB])(implicit builder: CanBuildSequence[A,B,ValA,ValB]): builder.OperatorSequenceBuilder = {
      import builder._
      builderSeq { head =>
        val left = p1 == head; val right = p2 == head
        val inx = if (left && right) true else false
        val prx = if (left && !right) true else false
        val psx = if (!left && right) true else false
        sequence(new Function2[Prec,Prec, o.SequenceBuilder] {
                   type Value = o.V
                   def apply(prec1: Prec, prec2: Prec) = AbstractParser.seq(p1(prec1), p2(prec2))
                   def infix = inx; def prefix = prx; def postfix = psx; def assoc = Assoc.UNDEFINED
                 })
      }
    }
    
    def seqOpSeqNt[A,B,ValA,ValB](p1: AbstractOperatorSequenceBuilder[A,ValA], p2: AbstractOperatorNonterminal[B,ValB])(implicit builder: CanBuildSequence[A,B,ValA,ValB]): builder.OperatorSequenceBuilder = {
      import builder._
      builderSeq { head => val q1 = p1(head)
        val left = q1.infix || q1.postfix; val right = p2 == head
        val inx = if (left && right) true else false
        val prx = if (left && !right) true else false
        val psx = if (!left && right) true else false
        sequence(new Function2[Prec,Prec, o.SequenceBuilder] {
                   type Value = o.V
                   def apply(prec1: Prec, prec2: Prec) = AbstractParser.seq(q1(prec1,$), p2(prec2))
                   def infix = inx; def prefix = prx; def postfix = psx; def assoc = Assoc.UNDEFINED
                 })
      }
    }
    
    def seqOpSeqSym[A,B,ValA](p1: AbstractOperatorSequenceBuilder[A,ValA], p2: AbstractSymbol[B])(implicit builder: CanBuildSequence[A,B,ValA,p2.Value]): builder.OperatorSequenceBuilder = {
      import builder._
      builderSeq { head => val q1 = p1(head)
        val psx = if (q1.infix || q1.postfix) true else false
        sequence(new Function2[Prec,Prec, o.SequenceBuilder] {
                   type Value = o.V
                   def apply(prec1: Prec, prec2: Prec) = AbstractParser.seq(q1(prec1,$), p2)
                   def infix = false; def prefix = false; def postfix = psx; def assoc = Assoc.UNDEFINED
                 })
      }
    }
    
    def seqNtSym[A,B,ValA](p1: AbstractOperatorNonterminal[A,ValA], p2: AbstractSymbol[B])(implicit builder: CanBuildSequence[A,B,ValA,p2.Value]): builder.OperatorSequenceBuilder = {
      import builder._
      builderSeq { head =>
        val psx = p1 == head
        sequence(new Function2[Prec,Prec, o.SequenceBuilder] {
                   type Value = o.V
                   def apply(prec1: Prec, prec2: Prec) = AbstractParser.seq(p1(prec1), p2)
                   def infix = false; def prefix = false; def postfix = psx; def assoc = Assoc.UNDEFINED
                 })
      }
    }
    
    def seqSymNt[A,B,ValB](p1: AbstractSymbol[A], p2: AbstractOperatorNonterminal[B,ValB])(implicit builder: CanBuildSequence[A,B,p1.Value,ValB]): builder.OperatorSequenceBuilder = {
      import builder._
      builderSeq { head =>
        val prx = if (p2 == head) true else false
        sequence(new Function2[Prec,Prec, o.SequenceBuilder] {
                   type Value = o.V 
                   def apply(prec1: Prec, prec2: Prec) = AbstractParser.seq(p1, p2(prec2))
                   def infix = false; def prefix = prx; def postfix = false; def assoc = Assoc.UNDEFINED
                 })
      }
    }
    
    def seqSeqNt[A,B,ValB](p1: AbstractSequenceBuilder[A], p2: AbstractOperatorNonterminal[B,ValB])(implicit builder: CanBuildSequence[A,B,p1.Value,ValB]): builder.OperatorSequenceBuilder = {
      import builder._
      builderSeq { head =>
        val prx = if (p2 == head) true else false
        sequence(new Function2[Prec,Prec, o.SequenceBuilder] {
                   type Value = o.V
                   def apply(prec1: Prec, prec2: Prec) = AbstractParser.seq(p1, p2(prec2))
                   def infix = false; def prefix = prx; def postfix = false; def assoc = Assoc.UNDEFINED
                 })
      }
    }
    
    def altOpAlt[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorAlternationBuilder[A,ValA], p2: AbstractOperatorAlternationBuilder[B,ValB])
                (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val (f2,opened2,closed2) = p2(head,group1); val (f1,opened1,closed1) = p1(head,opened2)
        val closed = closed2.orElse(closed1)
        ({ group2 => val q1 = f1(group2); val q2 = f2(closed1.getOrElse(group2))
             alternation { prec => AbstractParser.altAlt(q1(prec), q2(prec)) } }, opened1, closed)
      }
    }
    
    def altOpAltOpSeq[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorAlternationBuilder[A,ValA], p2: AbstractOperatorSequenceBuilder[B,ValB])
                     (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val s2 = p2(head)
        val (level,group) = if (s2.infix || s2.prefix || s2.postfix) group1.level(s2.assoc, if (s2.prefix) -1 else if (s2.postfix) 1 else 0)
                            else (-1, group1)
        val (f1,opened1,closed1) = p1(head,group)
        ({ group2 => val q1 = f1(group2); val q2 = filter(s2,level,closed1.getOrElse(group2))
             alternation { prec => AbstractParser.altAltSeq(q1(prec), q2(prec)) } }, opened1, closed1)
      }
    }
    
    def altOpAltOpSym[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorAlternationBuilder[A,ValA], p2: AbstractOperatorSymbol[B,ValB])
                     (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val (f1,opened1,closed1) = p1(head,group1)
        ({ group2 => val q1 = f1(group2)
             alternation { prec => AbstractParser.altAltSym(q1(prec), p2(prec)) } }, opened1, closed1)
      }
    }
    
    def altOpSeqOpAlt[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorSequenceBuilder[A,ValA], p2: AbstractOperatorAlternationBuilder[B,ValB])
                     (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val (f2,opened2,closed2) = p2(head,group1)
        val s1 = p1(head)
        val (level,group) = if (s1.infix || s1.prefix || s1.postfix) opened2.level(s1.assoc, if (s1.prefix) -1 else if (s1.postfix) 1 else 0)
                            else (-1, opened2)
        ({ group2 => val q1 = filter(s1,level,group2); val q2 = f2(group2)
             alternation { prec => AbstractParser.altSeqAlt(q1(prec), q2(prec)) } }, group, closed2)
      }
    }
    
    def altOpSeq[A,B >: A,ValA,ValB](p1: AbstractOperatorSequenceBuilder[A,ValA], p2: AbstractOperatorSequenceBuilder[B,ValB])
                (implicit builder: CanBuildAlternation[A,B,ValA,ValB], sub: ValA <:< ValB): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val s1 = p1(head); val s2 = p2(head)
        val (l2,g2) = if (s2.infix || s2.prefix || s2.postfix) group1.level(s2.assoc, if (s2.prefix) -1 else if (s2.postfix) 1 else 0)
                      else (-1, group1)
        val (l1,g1) = if (s1.infix || s1.prefix || s1.postfix) g2.level(s1.assoc, if (s1.prefix) -1 else if (s1.postfix) 1 else 0)
                      else (-1, g2)
        ({ group2 => val q1 = filter(s1,l1,group2); val q2 = filter(s2,l2,group2)
             alternation { prec => AbstractParser.altSeq(q1(prec), q2(prec)) } }, g1, None)
      }
    }
    
    def altOpSeqOpSym[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorSequenceBuilder[A,ValA], p2: AbstractOperatorSymbol[B,ValB])
                     (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val s1 = p1(head)
        val (l1,g1) = if (s1.infix || s1.prefix || s1.postfix) group1.level(s1.assoc, if (s1.prefix) -1 else if (s1.postfix) 1 else 0)
                      else (-1, group1)
        ({ group2 => val q1 = filter(s1,l1,group2)
             alternation { prec => AbstractParser.altSeqSym(q1(prec), p2(prec)) } }, g1, None)
      }
    }
    
    def altOpSymOpAlt[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorSymbol[A,ValA], p2: AbstractOperatorAlternationBuilder[B,ValB])
                     (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val (f2,opened2,closed2) = p2(head,group1)
        ({ group2 => val q2 = f2(group2)
             alternation { prec => AbstractParser.altSymAlt(p1(prec), q2(prec)) } }, opened2, closed2)
      }
    }
    
    def altOpSymOpSeq[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorSymbol[A,ValA], p2: AbstractOperatorSequenceBuilder[B,ValB])
                     (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val s2 = p2(head)
        val (l,g) = if (s2.infix || s2.prefix || s2.postfix) group1.level(s2.assoc, if (s2.prefix) -1 else if (s2.postfix) 1 else 0)
                      else (-1, group1)
        ({ group2 => val q2 = filter(s2,l,group2)
             alternation { prec => AbstractParser.altSymSeq(p1(prec), q2(prec)) } }, g, None)
      }
    }
    
    def altOpSym[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorSymbol[A,ValA], p2: AbstractOperatorSymbol[B,ValB])
                (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternation = { import builder._
      alternation { prec => AbstractParser.altSym(p1(prec), p2(prec)) }
    }
    
    def altSymOpSym[A,ValA](p: AbstractSymbol[A] { type Value = ValA }): AbstractOperatorSymbol[A,ValA] 
      = prec => p
    
    def altSeqOpSeq[A,ValA](p: AbstractSequenceBuilder[A] { type Value = ValA }): AbstractOperatorSequenceBuilder[A,ValA] 
      = head => new Function2[Prec,Prec,AbstractSequenceBuilder[A] { type Value = ValA }] {
                  def apply(prec1: Prec, prec2: Prec) = p
                  def infix = false; def prefix = false; def postfix = false; def assoc = Assoc.UNDEFINED
                }
    
    def altAltOpAlt[A,ValA](p: AbstractAlternationBuilder[A] { type Value = ValA }): AbstractOperatorAlternationBuilder[A,ValA]
      = (head, group1) => (group2 => prec => p, group1, None)
    
    /**
     * If |> is used inside an associativity group, it is ignored, i.e., is equivalent to use of |.
     */
    def greaterOpAlt[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorAlternationBuilder[A,ValA], p2: AbstractOperatorAlternationBuilder[B,ValB])
                    (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val (f2,opened2,closed2) = p2(head,group1)
        if (group1 subgroup) {
          val (f1,opened1,closed1) = p1(head,opened2)
          val closed = closed2.orElse(closed1)
          ({ group2 => val q1 = f1(group2); val q2 = f2(closed1.getOrElse(group2))
               alternation { prec => AbstractParser.altAlt(q1(prec), q2(prec)) } }, opened1, closed)
        } else {
          val (f1,opened1,closed1) = p1(head,opened2.group)
          val closed = Option(opened2.close)
          ({ group2 => val q1 = f1(group2); val q2 = f2(closed.getOrElse(group2))
               alternation { prec => AbstractParser.altAlt(q1(prec), q2(prec)) } }, opened1, closed)
        }
      }
    }
      
    def greaterOpSeqOpAlt[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorSequenceBuilder[A,ValA], p2: AbstractOperatorAlternationBuilder[B,ValB])
                         (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val (f2,opened2,closed2) = p2(head,group1)
        val s1 = p1(head)
        if (group1 subgroup) {
          val (level,group) = if (s1.infix || s1.prefix || s1.postfix) opened2.level(s1.assoc, if (s1.prefix) -1 else if (s1.postfix) 1 else 0)
                              else (-1, opened2)
          ({ group2 => val q1 = filter(s1,level,group2); val q2 = f2(group2)
               alternation { prec => AbstractParser.altSeqAlt(q1(prec), q2(prec)) } }, group, closed2)
        } else {
          val opened1 = opened2.group
          val (level,group) = if (s1.infix || s1.prefix || s1.postfix) opened1.level(s1.assoc, if (s1.prefix) -1 else if (s1.postfix) 1 else 0)
                              else (-1, opened1)
          val closed = Option(opened2.close)
          ({ group2 => val q1 = filter(s1,level,group2); val q2 = f2(closed.getOrElse(group2))
               alternation { prec => AbstractParser.altSeqAlt(q1(prec), q2(prec)) } }, opened1, closed)
        }
      }
    }
    
    def greaterOpAltOpSeq[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorAlternationBuilder[A,ValA], p2: AbstractOperatorSequenceBuilder[B,ValB])
                         (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val s2 = p2(head)
        val (level,group) = if (s2.infix || s2.prefix || s2.postfix) group1.level(s2.assoc, if (s2.prefix) -1 else if (s2.postfix) 1 else 0)
                            else (-1, group1)
        if (group1 subgroup) {
          val (f1,opened1,closed1) = p1(head,group)
          ({ group2 => val q1 = f1(group2); val q2 = filter(s2,level,closed1.getOrElse(group2))
               alternation { prec => AbstractParser.altAltSeq(q1(prec), q2(prec)) } }, opened1, closed1)
        } else {
          val (f1,opened1,closed1) = p1(head,group.group)
          val closed = Option(group.close)
          ({ group2 => val q1 = f1(group2); val q2 = filter(s2,level,closed.getOrElse(group2))
               alternation { prec => AbstractParser.altAltSeq(q1(prec), q2(prec)) } }, opened1, closed)
        }
      }
    }
    
    def greaterOpSeq[A,B >: A,ValA,ValB >: ValA](p1: AbstractOperatorSequenceBuilder[A,ValA], p2: AbstractOperatorSequenceBuilder[B,ValB])
                    (implicit builder: CanBuildAlternation[A,B,ValA,ValB]): builder.OperatorAlternationBuilder = { import builder._
      builderAlt { (head, group1) =>
        val s2 = p2(head)
        val (l2,g2) = if (s2.infix || s2.prefix || s2.postfix) group1.level(s2.assoc, if (s2.prefix) -1 else if (s2.postfix) 1 else 0)
                      else (-1, group1)
        val s1 = p1(head)
        if (group1 subgroup) {
          val (l1,g1) = if (s1.infix || s1.prefix || s1.postfix) g2.level(s1.assoc, if (s1.prefix) -1 else if (s1.postfix) 1 else 0)
                        else (-1, g2)
          ({ group2 => val q1 = filter(s1,l1,group2); val q2 = filter(s2,l2,group2)
               alternation { prec => AbstractParser.altSeq(q1(prec), q2(prec)) } }, g1, None)
        } else {
          val opened2 = g2.group
          val (l1,g1) = if (s1.infix || s1.prefix || s1.postfix) opened2.level(s1.assoc, if (s1.prefix) -1 else if (s1.postfix) 1 else 0)
                        else (-1, opened2)
          val closed = Option(g2.close)
          ({ group2 => val q1 = filter(s1,l1,group2); val q2 = filter(s2,l2,closed.getOrElse(group2))
               alternation { prec => AbstractParser.altSeq(q1(prec), q2(prec)) } }, g1, closed)
        }
      }
    }
    
    def assocAlt[A,ValA](implicit builder: CanBuildAlternation[A,A,ValA,ValA]): (builder.OperatorAlternationBuilder, Assoc.Assoc) => builder.OperatorAlternationBuilder = {
      import builder._
      { (p,a) => builderAlt { (head,group1) =>
            val (f,opened,closed) = p(head, Subgroup(a,group1)); val max = opened.max 
            ({ group2 => alternation(f(opened.close.asInstanceOf[Subgroup].update(group2))) }, group1.update(max), closed) } 
      }
    }
    
    def nonterminalSym[A,ValA](name: String, p: AbstractOperatorSymbol[A,ValA])(implicit builder: CanBuildNonterminal[A,ValA]): builder.OperatorNonterminal = {
      import builder._
      lazy val q: OperatorNonterminal = nonterminal(name, prec => AbstractCPSParsers.nonterminalSym (s"$name(${prec._1},${prec._2})", p(prec)) )
      q
    }
  
    def nonterminalSeq[A,ValA](name: String, p: => AbstractOperatorSequenceBuilder[A,ValA])(implicit builder: CanBuildNonterminal[A,ValA]): builder.OperatorNonterminal = {
      import builder._
      lazy val q: OperatorNonterminal = nonterminal(name, prec => AbstractCPSParsers.nonterminalSeq (s"$name(${prec._1},${prec._2})", p(q)(prec,prec)) )
      q
    }
  
    def nonterminalAlt[A,ValA](name: String, p: => AbstractOperatorAlternationBuilder[A,ValA])(implicit builder: CanBuildNonterminal[A,ValA]): builder.OperatorNonterminal = {
      import builder._
      lazy val q: OperatorNonterminal = nonterminal(name, {
        lazy val parser = { val (f,opened,closed) = p(q,Group()); f(opened.close) }
        prec => AbstractCPSParsers.nonterminalAlt (s"$name(${prec._1},${prec._2})", parser(prec)) 
      })
      q
    }
    
    def filter[A,ValA](p: AbstractOperatorSequence[A,ValA], l: Int, group: Group): Prec => AbstractSequenceBuilder[A] { type Value = ValA } = {
      println(s"Sequence with level: $l, group: $group, assoc: ${p.assoc}")
      if (l == -1) return prec => p(prec, prec)
      
      // Main condition
      val cond: Prec => Boolean = if (p.infix)       prec => group.parent.max >= prec._1 && group.parent.max >= prec._2
                                  else if (p.prefix) prec => group.parent.max >= prec._1
                                  else               prec => group.parent.max >= prec._2
                                  
      if (!group.climb(l)) {
        // Extra condition when climbing is not possible
        var l1 = 0; var r2 = 0
        val extra: Prec => Boolean = 
          if (!group.subgroup) {
            if (l == group.undef) { l1 = group.undef; r2 = group.undef; prec => true }
            else if (p.infix) {
              p.assoc match {
                case Assoc.LEFT => { l1 = group.undef; r2 = l; prec => prec._2 != l }
                case Assoc.RIGHT => { l1 = l; r2 = group.undef; prec => prec._1 != l }
                case Assoc.NON_ASSOC => { l1 = l; r2 = l; prec => prec._1 != l && prec._2 != l }
              }  
            } else if (p.prefix) {
              p.assoc match {
                case Assoc.NON_ASSOC => { r2 = l; prec => prec._2 != l }
              }
            } else {
              p.assoc match {
                case Assoc.NON_ASSOC => { l1 = l; prec => prec._1 != l }
              }
            }
          } else {
            if (p.infix)
              group.assoc match {
                case Assoc.LEFT =>  if (l == group.undef) { 
                                      l1 = group.parent.undef; r2 = group.undef; prec => !(group.min <= prec._2 && prec._2 <= group.max)
                                    } else p.assoc match {
                                      case Assoc.RIGHT => { l1 = l; r2 = l; prec => prec._1 != l && (prec._2 == l || !(group.min <= prec._2 && prec._2 <= group.max)) }
                                      case Assoc.NON_ASSOC => { l1 = l; r2 = l; prec => prec._1 != l && !(group.min <= prec._2 && prec._2 <= group.max) }
                                      case _ => throw new RuntimeException("Ups, this should not have happened!")
                                    }
                case Assoc.RIGHT => if (l == group.undef) {
                                      l1 = group.undef; r2 = group.parent.undef; prec => !(group.min <= prec._1 && prec._1 <= group.max)
                                    } else p.assoc match {
                                      case Assoc.LEFT => { l1 = l; r2 = l; prec => prec._2 != l && (prec._1 == l || !(group.min <= prec._1 && prec._1 <= group.max)) }
                                      case Assoc.NON_ASSOC => { l1 = l; r2 = l; prec => prec._2 != l && !(group.min <= prec._1 && prec._1 <= group.max) }
                                      case _ => throw new RuntimeException("Ups, this should not have happened!")
                                    }
                case Assoc.NON_ASSOC => if (l == group.undef) {
                                          l1 = group.undef; r2 = group.undef
                                          prec => !(group.min <= prec._1 && prec._1 <= group.max) && !(group.min <= prec._2 && prec._2 <= group.max)
                                        } else p.assoc match {
                                          case Assoc.LEFT => { l1 = l; r2 = group.undef; prec => (prec._1 == l || !(group.min <= prec._1 && prec._1 <= group.max)) && !(group.min <= prec._2 && prec._2 <= group.max) }
                                          case Assoc.RIGHT => { l1 = group.undef; r2 = l; prec => !(group.min <= prec._1 && prec._1 <= group.max) && (prec._2 == l || !(group.min <= prec._2 && prec._2 <= group.max)) }
                                          case _ => throw new RuntimeException("Ups, this should not have happened!")
                                        }
              }
            else if (p.prefix)
              group.assoc match {
                case Assoc.LEFT => if (l == group.undef) { r2 = group.undef; prec => true }
                                   else { r2 = l; prec => prec._2 != l } // NON_ASSOC case
                case Assoc.RIGHT => if (l == group.undef) { r2 = group.parent.undef; prec => !(group.min <= prec._1 && prec._1 <= group.max) }
                                    else { r2 = l; prec => prec._2 != l && !(group.min <= prec._1 && prec._1 <= group.max) } // NON_ASSOC case
                case Assoc.NON_ASSOC => { r2 = l; prec => !(group.min <= prec._1 && prec._1 <= group.max) && !(group.min <= prec._2 && prec._2 <= group.max) }
              }
            else
              group.assoc match {
                case Assoc.LEFT => if (l == group.undef) { l1 = group.parent.undef; prec => !(group.min <= prec._2 && prec._2 <= group.max) }
                                   else { l1 = l; prec => prec._1 != l && !(group.min <= prec._2 && prec._2 <= group.max) } // NON_ASSOC case
                case Assoc.RIGHT => if (l == group.undef) { l1 = group.undef; prec => true }
                                    else { l1 = l; prec => prec._1 != l } // NON_ASSOC case
                case Assoc.NON_ASSOC => { l1 = l; prec => !(group.min <= prec._1 && prec._1 <= group.max) && !(group.min <= prec._2 && prec._2 <= group.max) }
              }
          }
        
          val min = group.parent.min; val max = group.parent.max; val undef = group.parent.undef
          def choose(prec: Int) = if (min <= prec && prec <= max) undef else prec
        
          if (group.below.prefix && group.below.postfix)
            return prec => if (cond(prec) && extra(prec)) ??? else ??? //AbstractParser.alt(p((l1,choose(prec._2)),(choose(prec._1),r2))) else FAIL
          else if (group.below.prefix)
            return prec => if (cond(prec) && extra(prec)) p((l1,undef), (choose(prec._1),r2)) else FAIL[A,ValA]
          else if (group.below.postfix)
            return prec => if (cond(prec) && extra(prec)) p((l1,choose(prec._2)), (undef,r2)) else FAIL[A,ValA]
          else 
            return prec => if (cond(prec) && extra(prec)) p((l1,undef), (undef,r2)) else FAIL[A,ValA]
        
        } else {
          if (!group.subgroup || (group.subgroup && group.min == group.max)) {
            (if (!group.subgroup) p.assoc else group.assoc) match {
              case Assoc.UNDEFINED =>
                return if (group.below.prefix && group.below.postfix) prec => if (cond(prec)) p((l,prec._2), (prec._1,l)) else FAIL[A,ValA]
                       else if (group.below.prefix) prec => if (cond(prec)) p((l,l), (prec._1,l)) else FAIL[A,ValA]
                       else if (group.below.postfix) prec => if (cond(prec)) p((l,prec._2), (l,l)) else FAIL[A,ValA]
                       else prec => if (cond(prec)) p((l,l), (l,l)) else FAIL[A,ValA]
              case Assoc.LEFT => 
                return if (group.below.prefix && group.below.postfix) prec => if (cond(prec)) p((l,prec._2), (prec._1,l+1)) else FAIL[A,ValA]
                       else if (group.below.prefix) prec => if (cond(prec)) p((l,l), (prec._1,l+1)) else FAIL[A,ValA]
                       else if (group.below.postfix) prec => if (cond(prec)) p((l,prec._2), (l+1,l+1)) else FAIL[A,ValA]
                       else prec => if (cond(prec)) p((l,l), (l+1,l+1)) else FAIL[A,ValA]
              case Assoc.RIGHT => 
                return if (group.below.prefix && group.below.postfix) prec => if (cond(prec)) p((l+1,prec._2), (prec._1,l)) else FAIL[A,ValA]
                       else if (group.below.prefix) prec => if (cond(prec)) p((l+1,l+1), (prec._1,l)) else FAIL[A,ValA]
                       else if (group.below.postfix) prec => if (cond(prec)) p((l+1,prec._2), (l,l)) else FAIL[A,ValA]
                       else prec => if (cond(prec)) p((l+1,l+1), (l,l)) else FAIL[A,ValA]
              case Assoc.NON_ASSOC => 
                // TODO: extra condition for unary operators (non-assoc group that has unary cannot climb !!!)
                return if (group.below.prefix && group.below.postfix) prec => if (cond(prec)) p((l+1,prec._2), (prec._1,l+1)) else FAIL[A,ValA]
                       else if (group.below.prefix) prec => if (cond(prec)) p((l+1,l+1), (prec._1,l+1)) else FAIL[A,ValA]
                       else if (group.below.postfix) prec => if (cond(prec)) p((l+1,prec._2), (l+1,l+1)) else FAIL[A,ValA]
                       else prec => if (cond(prec)) p((l+1,l+1), (l+1,l+1)) else FAIL[A,ValA]
            } 
          } else {
            // TODO: for each level that is not equal to undef, add extra unequality constraints
            ???
          }
      }
    }
    
    def FAIL[A,ValA]: AbstractSequenceBuilder[A] { type Value = ValA } 
      = new Function1[Slot,AbstractSequence[A]] { 
          def apply(slot: Slot) = new AbstractParser[A] with Slot {
            def apply(input: Input, i: Int, sppfLookup: SPPFLookup): Result[A] = CPSResult.failure
            def size = 0
            def symbol = org.meerkat.tree.Sequence(org.meerkat.tree.Terminal("_FAILURE_"))
            def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, size)
          }
          type Value = ValA
          def action: Option[Any => Any] = None
        }
    
  }
  
  class Unary(val prefix: Boolean, val postfix: Boolean) { override def toString = s"unary($prefix, $postfix)" }
  
  object Unary { def apply() = new Unary(false, false) }
  
  class Group(val assoc: Assoc.Assoc, val min: Int, val max: Int, val undef: Int, 
              val here: Unary, val below: Unary) {
    
    def subgroup = false
    def parent = this
    
    def group =
      if (min == max || max - min == 1)
        new Group(assoc, max, max, -1, Unary(), new Unary(here.prefix || below.prefix, here.postfix || below.postfix))
      else
        new Group(assoc, max + 1, max + 1, -1, Unary(), new Unary(here.prefix || below.prefix, here.postfix || below.postfix))
    
    def update(max: Int) = new Group(assoc, min, max, undef, here, below)   
    def update(max: Int, undef: Int, here: Unary) = new Group(assoc, min, max, undef, here, below)
    
    def close = if (min == max) this 
                else if (max - min == 1) new Group(assoc, min, max - 1, undef, here, below)
                else {
                  // A group with different levels
                  if (undef != -1) new Group(assoc, min, max - 1, undef, here, below)
                  else new Group(assoc, min, max, max, here, below)
                }
    
    /**
     *  @param unary (-1 to indicate prefix, 1 to indicate postfix)
     *  Note: LEFT and RIGHT associativity have no effect on prefix and postfix operators 
     */
    def level(assoc: Assoc.Assoc, unary: Int = 0): (Int, Group) = {
      var undef = this.undef; var max = this.max
      val level = if (assoc == this.assoc || assoc == Assoc.UNDEFINED 
                        || ((unary == -1 || unary == 1) && assoc != Assoc.NON_ASSOC)) {
                    if (undef != -1) undef else { undef = max; max += 1; undef }
                  } else { max += 1; this.max }
      val here = new Unary(if (unary == -1) true else this.here.prefix, 
                           if (unary == 1) true else this.here.postfix)
      (level, this.update(max, undef, here))
    }
    
    def climb(level: Int) = if (min == max) true
                            else false
                            
    override def toString = s"Group($assoc, $min, $max, $undef, $here, $below)"
    
  }
  
  object Group {
    def apply() = new Group(Assoc.UNDEFINED,1,1,-1,Unary(),Unary())
  }
  
  class Subgroup(override val assoc: Assoc.Assoc, override val min: Int, override val max: Int, override val undef: Int,
                 override val here: Unary, override val below: Unary,
                 override val parent: Group) extends Group(assoc, min, max, undef, here, below) {
    
    override def subgroup = true
    
    override def group = new Subgroup(assoc, max, max, undef, 
                           Unary(), new Unary(here.prefix || below.prefix, here.postfix || below.postfix), 
                           parent)
    
    override def update(max: Int) = new Subgroup(assoc, min, max, undef, here, below, parent)    
    override def update(max: Int, undef: Int, here: Unary) = new Subgroup(assoc, min, max, undef, here, below, parent)
    
    override def close = if (min == max) this else new Subgroup(assoc, min, max - 1, undef, here, below, parent)
    
    def update(parent: Group) = new Subgroup(assoc, min, max, undef, here, below, parent)
    
    override def climb(level: Int) = {
      if (this.min == parent.min && this.max == parent.max 
            && level == this.undef)
        true
      else false
    }
    
    override def toString = s"Subgroup($assoc,$min,$max,$undef,$here,$below,$parent)"
    
  } 
  
  object Subgroup {
    def apply(assoc: Assoc.Assoc, parent: Group) = new Subgroup(assoc, parent.max, parent.max, -1, parent.here, parent.below, parent)
  }
  
}