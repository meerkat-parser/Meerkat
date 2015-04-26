/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper

import meerkat._
import util.JavaTokens._

object IMAP extends MeerkatParsers {
  
  /**
   * Data dependent parsing
   *
   */
  
  val Number: MeerkatParser = "Number" ::= """0|[1-9][0-9]*""".r
  val OCTET: MeerkatParser = "OCTET" ::= """.""".r
  
  val Number2: MeerkatDDParser[Int] = Number.toStr.map(s => s.toInt)
  
  val literal8: MeerkatDDParser[Unit] 
    = "~{" ~^ Number2 ~> "+".? ~> "}" ~^ (n => OCTET.*(n))
  
  val L8: MeerkatParser   = "Literal8" ::= literal8
      
  import util.Configuration._
  
  def main(args: Array[String]): Unit = {
      parse("~{ 5 } X X X X X", L8, ALL_PARSES, NO_TESTING)
    }

}