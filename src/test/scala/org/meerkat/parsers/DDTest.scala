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

package org.meerkat.parsers

import org.meerkat.Syntax._
import org.meerkat.parsers._
import DDParsers._
import Parsers._
import DefaultLayout._

object DDTest {
  
  object Test1 {
    
    val Num = syn { "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" }
    
    val E: DataNonterminal[String] = syn { ( E ~ "+" ~ E ).map { case x~y => s"(${x+y})" } | Num.map { s => s } }
    
    val Octet = syn { """.""".r }
    
    val L8 = syn { "~{" ~ Num.map { _.toInt } ~"}" ~>> { Octets(_,Octet) } }
    
    def Octets(n: Int, p: Nonterminal) 
      = n match {
          case 0 => syn { Ø }
          case 1 => syn { p }
          case _ => syn { (1 to n-2).foldLeft(p ~ p)((q,_) => q ~ p) }
        }
    
    def main(args: Array[String]): Unit = {
      parse("~{ 9 } X X X X X X X X X",L8)
    }     
    
  }
  
  def main(args: Array[String]): Unit = {
    Test1.main(args)
  }
  
}