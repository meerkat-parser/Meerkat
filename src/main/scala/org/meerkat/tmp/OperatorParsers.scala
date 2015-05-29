//package org.meerkat.tmp
//
//import org.meerkat.sppf.NonPackedNode
//import org.meerkat.sppf.SPPFLookup
//import org.meerkat.util.Input
//import java.util.HashMap
//import org.meerkat.sppf.DefaultSPPFLookup
//import org.meerkat.util.Visualization
//
//object OperatorParsers {
//  
//  import AbstractOperatorParsers._
//  import Parsers._
//  
//  object OperatorImplicits {
//  
//    implicit object obj1 extends CanBuildSequence[NonPackedNode, NonPackedNode] {
//      implicit val obj = Parsers.obj1
//      implicit val m1 = Parsers.obj3
//      implicit val m2 = Parsers.obj3
//      
//      type OperatorSequence = OperatorParsers.OperatorSequence
//      
//      def sequence(p: AbstractOperatorSequence): OperatorSequence 
//        = new OperatorSequence { 
//            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
//            def infix = p.infix; def prefix = p.prefix; def postfix = p.postfix
//            def assoc = p.assoc
//          }
//      
//      def assoc(p: OperatorSequence, a: Assoc.Assoc): OperatorSequence 
//        = if ((p.infix && a != Assoc.NON_ASSOC) 
//                || ((p.infix || p.prefix || p.postfix) && a == Assoc.NON_ASSOC)) 
//            new OperatorSequence {
//              def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
//              def infix = p.infix; def prefix = p.prefix; def postfix = p.postfix
//              def assoc: Assoc.Assoc = a
//            } 
//        else p
//      
//      type OperatorSequenceBuilder = OperatorParsers.OperatorSequenceBuilder
//      def builderSeq(f: Head => OperatorSequence): OperatorSequenceBuilder
//        = new OperatorSequenceBuilder { def apply(head: Head) = f(head) }
//    }
//    
//    implicit object obj2 extends CanBuildAlternation[NonPackedNode, NonPackedNode] {
//      implicit val obj1 = Parsers.obj2
//      implicit val obj2 = Parsers.obj2
//      implicit val m1 = Parsers.obj3
//      implicit val m2 = Parsers.obj3
//      
//      type OperatorAlternation = OperatorParsers.OperatorAlternation     
//      def alternation(f: Prec => obj2.AlternationBuilder): OperatorAlternation 
//        = new OperatorAlternation { def apply(prec: Prec) = f(prec) }
//      
//      type OperatorAlternationBuilder = OperatorParsers.OperatorAlternationBuilder
//      def builderAlt(f: (Head, Group) => (Group => OperatorAlternation, Group, Option[Group])): OperatorAlternationBuilder
//        = new OperatorAlternationBuilder { def apply(head: Head, group: Group) = f(head, group) }
//    }
//    
//    implicit object obj3 extends CanBuildNonterminal[NonPackedNode] {
//      implicit val obj1 = Parsers.obj4
//      implicit val obj2 = Parsers.obj2
//      
//      type OperatorNonterminal = OperatorParsers.OperatorNonterminal
//    
//      def nonterminal(name: String, f: Prec => obj1.Nonterminal): OperatorNonterminal
//        = new OperatorNonterminal {
//            val table: java.util.Map[Prec, Parsers.Nonterminal] = new HashMap()
//            def apply(prec: Prec) = if (table.containsKey(prec)) table.get(prec) 
//                                    else {
//                                      val nt = f(prec)
//                                      table.put(prec, nt)
//                                      nt
//                                    }
//            override def toString = name
//          }
//    }
//  
//  }
//  
//  trait OperatorSequence extends ((Prec, Prec) => Parsers.SequenceBuilder) {
//    def infix: Boolean; def prefix: Boolean; def postfix: Boolean
//    def assoc: Assoc.Assoc
//  }
//  
//  trait OperatorAlternation extends (Prec => Parsers.AlternationBuilder)
//  
//  trait OperatorNonterminal extends (Prec => Parsers.Nonterminal) { import OperatorImplicits._; import AbstractOperatorParser._
//    def ~ (p: OperatorNonterminal) = seqNt(this, p)
//    def ~ (p: Symbol) = seqNtSym(this, p)
//    
//    def | (p: OperatorAlternationBuilder) = altOpSymOpAlt(this, p)
//    def | (p: OperatorSequenceBuilder) = altOpSymOpSeq(this, p)
//    def | (p: OperatorNonterminal) = altOpSym(this, p)
//    
//    def | (p: AlternationBuilder) = altOpSymOpAlt(this, altAltOpAlt(p))
//    def | (p: SequenceBuilder) = altOpSymOpSeq(this, altSeqOpSeq(p))
//    def | (p: Symbol) = altOpSym(this, altSymOpSym(p))
//        
//  }
//  
//  trait OperatorSequenceBuilder extends (Head => OperatorSequence) { import OperatorImplicits._; import AbstractOperatorParser._
//    def ~ (p: OperatorNonterminal) = seqOpSeqNt(this, p)
//    def ~ (p: Symbol) = seqOpSeqSym(this, p)
//    
//    def | (p: OperatorAlternationBuilder) = altOpSeqOpAlt(this, p)
//    def | (p: OperatorSequenceBuilder) = altOpSeq(this, p)
//    def | (p: OperatorNonterminal) = altOpSeqOpSym(this, p)
//    
//    def | (p: AlternationBuilder) = altOpSeqOpAlt(this, altAltOpAlt(p))
//    def | (p: SequenceBuilder) = altOpSeq(this, altSeqOpSeq(p))
//    def | (p: Symbol) = altOpSeqOpSym(this, altSymOpSym(p))
//    
//    def |> (p: OperatorAlternationBuilder) = greaterOpSeqOpAlt(this, p)
//    def |> (p: OperatorSequenceBuilder) = greaterOpSeq(this, p)
//  }
//  
//  trait OperatorAlternationBuilder extends ((Head, Group) => (Group => OperatorAlternation, Group, Option[Group])) { import OperatorImplicits._; import AbstractOperatorParser._
//    def | (p: OperatorAlternationBuilder) = altOpAlt(this, p)
//    def | (p: OperatorSequenceBuilder) = altOpAltOpSeq(this, p)
//    def | (p: OperatorNonterminal) = altOpAltOpSym(this, p)
//    
//    def | (p: AlternationBuilder) = altOpAlt(this, altAltOpAlt(p))
//    def | (p: SequenceBuilder) = altOpAltOpSeq(this, altSeqOpSeq(p))
//    def | (p: Symbol) = altOpAltOpSym(this, altSymOpSym(p))
//    
//    def |> (p: OperatorAlternationBuilder) = greaterOpAlt(this, p)
//    def |> (p: OperatorSequenceBuilder) = greaterOpAltOpSeq(this, p)
//  }
//  
//  implicit class ParsersSeqOps(p: Symbol) { import OperatorImplicits._; import AbstractOperatorParser._
//    def ~ (q: OperatorNonterminal) = seqSymNt(p, q)
//    
//    def | (q: OperatorAlternationBuilder) = altOpSymOpAlt(altSymOpSym(p), q)
//    def | (q: OperatorSequenceBuilder) = altOpSymOpSeq(altSymOpSym(p), q)
//    def | (q: OperatorNonterminal) = altOpSym(altSymOpSym(p), q)
//  }
//  
//  implicit class ParsersAltOps1(p: SequenceBuilder) { import OperatorImplicits._; import AbstractOperatorParser._
//    def ~ (q: OperatorNonterminal) = seqSeqNt(p, q)
//    
//    def | (q: OperatorAlternationBuilder) = altOpSeqOpAlt(altSeqOpSeq(p), q)
//    def | (q: OperatorSequenceBuilder) = altOpSeq(altSeqOpSeq(p), q)
//    def | (q: OperatorNonterminal) = altOpSeqOpSym(altSeqOpSeq(p), q)
//  }
//  
//  implicit class ParsersAltOps2(p: AlternationBuilder) { import OperatorImplicits._; import AbstractOperatorParser._
//    def | (q: OperatorAlternationBuilder) = altOpAlt(altAltOpAlt(p), q)
//    def | (q: OperatorSequenceBuilder) = altOpAltOpSeq(altAltOpAlt(p), q)
//    def | (q: OperatorNonterminal) = altOpAltOpSym(altAltOpAlt(p), q)
//  }
//  
//  implicit class StringSeqOps(term: String) { import OperatorImplicits._; import AbstractOperatorParser._
//    def ~ (q: OperatorNonterminal) = seqSymNt(Parsers.toTerminal(term), q) 
//  }
//  
//  implicit class StringAltOps(term: String) { import OperatorImplicits._; import AbstractOperatorParser._
//    val p: Symbol = term
//    
//    def | (q: OperatorAlternationBuilder) = altOpSymOpAlt(altSymOpSym(p), q)
//    def | (q: OperatorSequenceBuilder) = altOpSymOpSeq(altSymOpSym(p), q)
//    def | (q: OperatorNonterminal) = altOpSym(altSymOpSym(p), q)
//  }
//  
//  def left(p: OperatorSequenceBuilder): OperatorSequenceBuilder = { import OperatorImplicits._
//    obj1.builderSeq(head => obj1.assoc(p(head), Assoc.LEFT))
//  }
//  
//  def right(p: OperatorSequenceBuilder): OperatorSequenceBuilder = { import OperatorImplicits._
//    obj1.builderSeq(head => obj1.assoc(p(head), Assoc.RIGHT))
//  }
//  
//  def non_assoc(p: OperatorSequenceBuilder): OperatorSequenceBuilder = { import OperatorImplicits._
//    obj1.builderSeq(head => obj1.assoc(p(head), Assoc.NON_ASSOC))
//  }
//  
//  def left(p: OperatorAlternationBuilder): OperatorAlternationBuilder = { import OperatorImplicits._; import AbstractOperatorParser.assocAlt
//    assocAlt(obj2)(p, Assoc.LEFT)
//  }
//  
//  def right(p: OperatorAlternationBuilder): OperatorAlternationBuilder = { import OperatorImplicits._; import AbstractOperatorParser.assocAlt
//    assocAlt(obj2)(p, Assoc.RIGHT)
//  }
//  
//  def non_assoc(p: OperatorAlternationBuilder): OperatorAlternationBuilder = { import OperatorImplicits._; import AbstractOperatorParser.assocAlt
//    assocAlt(obj2)(p, Assoc.NON_ASSOC)
//  }
//  
//  def ntAlt(name: String, p: => OperatorAlternationBuilder): OperatorNonterminal = { import OperatorImplicits._; import AbstractOperatorParser.nonterminalAlt
//    implicit val m = Parsers.obj3
//    nonterminalAlt(name, p)
//  }
//  def ntSeq(name: String, p: => OperatorSequenceBuilder): OperatorNonterminal = { import OperatorImplicits._; import AbstractOperatorParser.nonterminalSeq
//    implicit val m = Parsers.obj3
//    nonterminalSeq(name, p)
//  }
//  def ntSym(name: String, p: OperatorNonterminal): OperatorNonterminal = { import OperatorImplicits._; import AbstractOperatorParser.nonterminalSym
//    implicit val m = Parsers.obj3
//    nonterminalSym(name, p)
//  }
//    
//  def run(input: Input, sppf: SPPFLookup, parser: AbstractCPSParsers.AbstractParser[NonPackedNode]): Unit = {
//    parser(input, 0, sppf)(t => if(t.rightExtent == input.length) { println(s"Success: $t")  })
//    Trampoline.run
//  }
//  
//  def parse(sentence: String, parser: OperatorNonterminal): Unit = {
//    val input = new Input(sentence)
//    val sppf = new DefaultSPPFLookup(input)
//    
//    val p = parser((0,0))
//    run(input, sppf, p)
//    
//    println(s"Trying to find: ${p.name}(0,${sentence.length()})")
//    val startSymbol = sppf.getStartNode(p, 0, sentence.length())
//    
//    startSymbol match {
//      case None       => println("Parse error")
//      case Some(node) => println("Success: " + node)
//                         println(sppf.countAmbiguousNodes + ", " + sppf.countIntermediateNodes + ", " + sppf.countPackedNodes + ", " + sppf.countNonterminalNodes + ", " + sppf.countTerminalNodes)
//                         println("Visualizing...") 
//                         Visualization.visualize(Visualization.toDot(startSymbol.get), "sppf")
//                         println("Done!")
//    }
//  }
//  
//}