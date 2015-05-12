package org.meerkat.tmp

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
     
    private var rec: Rec.Rec = Rec.UNDEFINED
    private var assoc: Assoc.Assoc = Assoc.UNDEFINED
    
    def precedence: Precedence = throw new RuntimeException("Not implemented!") 
    
    def defineRecursion(rec: Rec.Rec): Unit = this.rec = rec
    def defineAssoc(assoc: Assoc.Assoc): Unit = this.assoc = assoc
     
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
  
  class Group(val assoc: Assoc.Assoc, val min: Int) {
    
    private var max: Int = -1
    
    def getMax = max
    def assignMax(min: Int): Unit = this.max = max
  }
  
  class Precedence {
    
    private var count = 1
    private var group: Group = _
    
    def counter: Int = this.count
    
    def incr: Int = { this.count += 1; this.count }
    
    def newGroup(assoc: Assoc.Assoc): Unit = this.group = new Group(assoc, count)
    
    def finaliseCurrentGroup: Group = {
      this.group.assignMax(count)
      this.group
    }
  }

}