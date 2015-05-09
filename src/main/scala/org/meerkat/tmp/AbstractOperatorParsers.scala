package org.meerkat.tmp

object Rec extends Enumeration {
  type Rec = Value
  val UNKNOWN, NON, LEFT, RIGHT, BOTH = Value
}

object Group extends Enumeration {
  type Group = Value
  val UNKNOWN, NON, LEFT, RIGHT, ASSOC, NON_ASSOC = Value
}

object AbstractOperatorParsers {
  
  import AbstractCPSParsers._
  
  type Prec = (Int, Int)
  
  val $: Prec = (0,0)
  
  trait AbstractOperatorParser[+T] extends (Prec => AbstractParser[T]) {
     
    private var rec: Rec.Rec = Rec.UNKNOWN
    private var group: Group.Group = Group.UNKNOWN
    
    def defineRecursion(rec: Rec.Rec): Unit = this.rec = rec
    def defineGroup(group: Group.Group): Unit = this.group = group
     
    def isLeft = rec == Rec.LEFT || rec == Rec.BOTH
    def isRight = rec == Rec.RIGHT || rec == Rec.BOTH
    def isLeftOrRight = Rec.BOTH
    
    private var l = 0
    def assign(l: Int): Unit = this.l = l
    def level: Int = l
    
    private var hd: AbstractOperatorParser[Any] = _
    def head: AbstractOperatorParser[Any] = hd
    def headed(head: AbstractOperatorParser[Any]): AbstractOperatorParser[T] = { hd = head; this }
    
    def isSequence = false
    def isAlternation = false
    
    def isNonterminal = false
    
  }

}