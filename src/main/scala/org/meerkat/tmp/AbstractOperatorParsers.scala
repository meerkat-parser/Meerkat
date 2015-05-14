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
    
    def level: Int = ???
    def pass(group: Group): Unit = ???
    def pass(head: AbstractOperatorParser[Any]): Unit = ???
    
    def isSequence = false
    def isAlternation = false
    
    def isNonterminal = false
    
  }
  
  class Group(val assoc: Assoc.Assoc, min: Int) {
    
    private var max: Int = min
    
    def minimum = min
    def maximum = max
    
    def startNew(assoc: Assoc.Assoc) = new Group(assoc, max + 1)
    
    private var assocs: List[Group] = _
  }
  
  object Group {
    def apply() = new Group(Assoc.UNDEFINED, 1)
  }
  
}