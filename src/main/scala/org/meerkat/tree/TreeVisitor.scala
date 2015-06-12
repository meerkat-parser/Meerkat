/*
 * Copyright (c) 2015, Anastasia Izmaylova and Ali Afroozeh, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */

package org.meerkat.tree

import org.meerkat.util.Input
import scala.collection.mutable.HashMap
import org.meerkat.util.visualization.Shape._
import org.meerkat.util.visualization.Style._
import org.meerkat.util.visualization.Color._
import org.meerkat.util.visualization._

trait TreeVisitor {
  type T
  def visit(t: Tree)(implicit input: Input): T  
}

class TreeToDot extends TreeVisitor {
    type T = Unit
    
    val sb = new StringBuilder
    
    def get: String = sb.toString
    
    def visit(t: Tree)(implicit input: Input): T = t match {
      case n @ Epsilon()   => sb ++= getShape(n.id, "&epsilon;", Rectangle, Rounded)
    
      case n @ Terminal(s) => sb ++= getShape(n.id, "\"" + s + "\"", Rectangle, Rounded)
    
      case n @ Appl(r, s)  => {
        r match {
          case r: Rule        => sb ++= getShape(n.id, if (r.head.isRegular) s"${r.head}" else s"$r", Rectangle, Rounded)
          case r: PartialRule => sb ++= getShape(n.id, s"$r", Rectangle)
          case r: RegularRule => sb ++= getShape(n.id, s"$r", Rectangle)
        }
        s.foreach { t => addEdge(n.id, t.id, sb); visit(t) }      
      }
    
      case n @ Amb(s)      => {
        sb ++= getShape(n.id, "Amb", Diamond, color = Red)
        s.foreach { t => addEdge(n.id, t.id, sb); visit(t) }
      } 
    }
}