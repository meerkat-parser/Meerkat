/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
import sppf._
import scala.util.matching.Regex
import scala.util.control.Breaks._
import meerkat.Result._
import util.Input

package object meerkat {
  
  type Parser = (Input, SPPFLookup, Int) => Result[NonPackedNode]
  type DDParser[T] = (Input, SPPFLookup, Int) => Result[(NonPackedNode, T)]
  
  implicit def mkHead(s: String): Head = new Head(s)
  
  val epsilon: TerminalParser = terminal((input, sppf, i) => success(sppf.getEpsilonNode(i)), "epsilon")
  
  def terminal(f: (Input, SPPFLookup, Int) => Result[NonPackedNode], tn: String): TerminalParser = {
    val p = new TerminalParser { def apply(input: Input, sppf: SPPFLookup, i: Int): Result[NonPackedNode] = f(input, sppf, i) }
    p.nameAs(tn)
    p
  }
  
  implicit def terminal(word: String): TerminalParser 
    = terminal((input, sppf, i) => if(input.startsWith(word, i))
                                     success(sppf.getTerminalNode(word, i, i + word.length()))
                                   else failure, word)

  implicit def terminal(c: Char): TerminalParser = {
    terminal((input, sppf, i) => if(i < input.length && input.charAt(i) == c)
                                   success(sppf.getTerminalNode(c, i))
                                 else failure, c.toString)
  }
  
  implicit def terminal(r: Regex): TerminalParser
    = terminal((input, sppf, i) => { val end = input.matchRegex(r, i)
                                     if(end != -1) success(sppf.getTerminalNode(r.toString, i, end))
                                     else failure }, r.toString)
  
  implicit def range(c: Char): StartChar = new StartChar(c)
  
  def charClass(chars: Char*)(ranges: (Char, Char)*) 
    = terminal((input, sppf, i) => if(i >= input.length) failure
                                   else {
                                     val c = input.charAt(i)
                                     if(chars.contains(c))
                                       success(sppf.getTerminalNode(c, i))
                                     else {
                                       var found: Boolean = false
                                       breakable {
                                         for(range <- ranges)
                                           if(c >= range._1 && c <= range._2) {
                                             found = true
                                             break
                                           }
                                       }
                                       if(found) success(sppf.getTerminalNode(c, i))
                                       else failure
                                     }
                                   }, chars.toString + ranges.toString)
  
  implicit def ddterminal(s: String): MeerkatDDParser[Unit] = {
    val p: MeerkatParser = s
    val q: MeerkatDDParser[Unit] = p
    q
  }
  
  def start(parser: MeerkatParser)(implicit l: Layout): MeerkatParser 
    = "start[" + parser.name.value + "]" ::= l.parser ~~ parser ~~ l.parser
  
}