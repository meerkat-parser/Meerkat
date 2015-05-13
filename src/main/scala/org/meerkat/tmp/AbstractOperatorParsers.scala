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
  
  class Groups[+T](val recs: List[AbstractOperatorParser[T]], 
                  val at: List[Int], 
                  val leftAt: List[Int], 
                  val rightAt: List[Int])
  
  trait AbstractOperatorParser[+T] extends (Prec => AbstractParser[T]) {
     
    def precedence: Precedence = throw new RuntimeException("Not implemented!") 
    
    def isLeft = false // declared to be left
    def isRight = false // declared to be right
    
    def isLeft[U](p: AbstractOperatorParser[U]) = false
    def isRight[U](p: AbstractOperatorParser[U]) = false
    
    private var l = 0
    def assign(l: Int): Unit = this.l = l
    def prLevel: Int = l
    
    private var group: Group = _
    def assign(group: Group): Unit = this.group = group
    def prGroup: Group = group
    
    private var hd: AbstractOperatorParser[Any] = _
    def head: AbstractOperatorParser[Any] = hd
    def headed(head: AbstractOperatorParser[Any]): AbstractOperatorParser[T] = { hd = head; this }
    
    def isSequence = false
    def isAlternation = false
    
    def isNonterminal = false
    
    def or[U >: T](alt: AbstractOperatorParser[U]): Groups[U] = {
      val gr = this.asGroups
      
      val recs = gr.recs.:+(alt)
      val leftAt  = if (alt isLeft)  gr.leftAt.:+(gr.recs.length + 1)  else gr.leftAt
      val rightAt = if (alt isRight) gr.rightAt.:+(gr.recs.length + 1) else gr.rightAt
      new Groups(recs, gr.at, leftAt, rightAt)
    }
      
    def greater[U >: T](alt: AbstractOperatorParser[U]): Groups[U] = {
      val gr = this.asGroups
      
      val recs = gr.recs.:+(alt)
      val leftAt  = if (alt isLeft)  gr.leftAt.:+(recs.length)  else gr.leftAt
      val rightAt = if (alt isRight) gr.rightAt.:+(recs.length) else gr.rightAt
      new Groups(recs, gr.at.:+(recs.length), leftAt, rightAt)
    }
      
    def asGroups: Groups[T] 
      = new Groups(List(this), List(0), if (isLeft) List() else List(0), if (isRight) List() else List(0))
    
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
    
    def newGroup(assoc: Assoc.Assoc): Group = {
      finaliseCurrentGroup
      incr
      this.group = new Group(assoc, count)
      this.group
    }
    
    def finaliseCurrentGroup: Group = {
      this.group.assignMax(count)
      this.group
    }
  }

}