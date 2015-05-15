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
     
    def isLeftRec = false
    def isRightRec = false
    
    def level: Int = -1
    def assoc: Assoc.Assoc = Assoc.UNDEFINED
    def pass(group: Group): Unit = ???
    def pass(head: AbstractOperatorParser[Any]): Unit = ???
    
    def isSequence = false
    def isAlternation = false
    
    def isNonterminal = false
    
  }
  
  class Group(val assoc: Assoc.Assoc, min: Int) {
    
    private var max: Int = min
    private var undef: Int = -1 
    
    def minimum = min
    def maximum = max
    
    def startNew(assoc: Assoc.Assoc) = {
      finalise
      new Group(assoc, max + 1)
    }
    
    def finalise = if (max != min) max -= 1
    
    def get(assoc: Assoc.Assoc): Int = {
      if (assoc == Assoc.UNDEFINED && undef == -1) {
        undef = max
        max += 1
        undef
      } else if (assoc == Assoc.UNDEFINED) {
        undef
      } else {
        val cur = max
        max += 1
        cur
      }
    }
    
    def canClimb(level: Int): Boolean = min == max
        
    private var assocs: List[Group] = _
  }
  
  object Group {
    def apply() = new Group(Assoc.UNDEFINED, 1)
  }
  
}