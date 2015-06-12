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

package org.meerkat.parsers.examples

import org.meerkat.Syntax._
import org.meerkat.parsers._
import Parsers._

/**
 * EBNF (sequences, option), character-level
 */

object Example2 { import Example1._
  
  val StarSep1: Nonterminal & List[String] = syn { S.*(",")  & { _.:+("!") }}                                
  val Star1   : Nonterminal & List[String] = syn { S.*       & { _.:+("!") }}
  val Plus1   : Nonterminal & List[String] = syn { S.+       & { _.:+("!") }}
  val Opt1    : Nonterminal & List[String] = syn { S.?       & { _.:+("!") }}
  val Group1  : Nonterminal & String       = syn { (A ~ B).! & { case (x,y) => x.concat("~").concat(y) }}
  
  val C = syn { "c" }
  val D = syn { "d" }
  val P = syn ( C ~ D | "c" )
  
  val Star2 : Nonterminal = syn { P.** }
  val Plus2 : Nonterminal = syn { P.+ } 
  val Opt2  : Nonterminal = syn { P.? }
  val Group2: Nonterminal = syn { (C ~ D).! }
    
  val Char1 = syn { P \ "cdd" }
  val Char2 = syn { P.* !>> "cd" }
  
  val Q = syn { C ~ D.? }
  
  def test1 = parse("ab,ab,ab"   , StarSep1)
  
  def test2 = parse("ababab"     , Star1)
  def test3 = parse("cdcdcd"     , Star2)
  
  def test4 = parse("ab"         , Opt1)
  def test5 = parse("cd"         , Opt2)
  
  def test6 = parse("cd cd cd cd", Char2)
  
  def test7 = parse("a b"        , Group1)
  def test8 = parse("c d"        , Group2)
  
  def test9 = parse("cd", Q)
  
  def main(args: Array[String]): Unit = test9
  
}