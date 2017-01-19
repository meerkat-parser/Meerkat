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
import org.scalatest.FunSuite

/**
 * EBNF (sequences, option), character-level
 */
class Example2 extends FunSuite {
  
  val A = syn { "a" ^ toStr }
  val B = syn { "b" ^ toStr }

  
  val S = 
  syn ( A ~ B  & { case x~y => s"$x++$y" } 
      | "c"    ^ { toStr } 
      )  
  val StarSep1: Nonterminal & List[String] = syn { S.*(",")  & { _.:+("!") }}                                
  val Star1   : Nonterminal & List[String] = syn { S.*       & { _.:+("!") }}
  val Plus1   : Nonterminal & List[String] = syn { S.+       & { _.:+("!") }}
  val Opt1    : Nonterminal & List[String] = syn { S.?       & { _.:+("!") }}
  val Group1  : Nonterminal & String       = syn { (A ~ B).! & { case x~y => x.concat("~").concat(y) }}
  
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
  
  test("test1") {
    val result = exec(StarSep1, "ab,ab,ab")
    assert(result.isSuccess)
    assert(result.asSuccess == List("a++b", "a++b", "a++b", "!"))
  }
 
  test("test2") {
	  val result = parse(Star1, "ababab")
    assert(result.isSuccess)
  }
  
  test("test3") {
    val result = parse(Star2, "cdcdcd")
    assert(result.isSuccess)
  }
  
  test("test4") {
    val result = parse(Opt1, "ab")
    assert(result.isSuccess)
  }
  
  test("test5") {
     val result = parse(Opt2, "cd")
     assert(result.isSuccess)
  }
  
  test("test6") {
    val result = parse(Char2, "cd cd cd cd")
    assert(result.isSuccess)
  } 
  
  test("test7") {
    val result = parse(Group1, "a b")
    assert(result.isSuccess)
  }
  
  test("test8") {
   val result = parse(Group2, "c d")
   assert(result.isSuccess)
  }
  
  test("test9") {
    val result = parse(Q, "cd")
    assert(result.isSuccess)
  }
  
}