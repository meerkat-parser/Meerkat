/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package meerkat

import sppf.NonPackedNode
import Result.memo
import util.Input
import sppf.SPPFLookup

class Head(head: String) {
  def ::= (parser: => MeerkatParser): Rule = new Rule(head, parser.rule(head))
}

class Rule(head: String, parser: => MeerkatParser) {
  var p: MeerkatParser = null
  var eval = 0
    
  def getParser: MeerkatParser = {
    if(eval == 0) {
      eval = 1
      p = parser
    }
    p
  }
  
  def getHead: String = this.head
}

object Rule {
  
  implicit def memoize(r: Rule): MeerkatParser = {
    var table: Array[Result[NonPackedNode]] = null
    val p = new MeerkatParser {
              def apply(input: Input, sppf: SPPFLookup, i: Int): Result[NonPackedNode]= {
                if(table == null) {
                  table = new Array(input.length + 1)
                }
                val result = table(i)
                if(result == null) {
                  table(i) = memo(r.getParser(input, sppf, i))
                  table(i)
                } else {
                  result
                }
                
              }
            }
    p.nameAs(r.getHead)
    p.resetWith(if(table != null) { table = null; r.getParser.reset() }) 
    p
  }
}
