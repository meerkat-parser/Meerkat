/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper

import meerkat._

object Offside extends MeerkatParsers {
  
  val Exp: MeerkatParser = "Exp" ::= 'e'
    
  def stat1(c: Int): MeerkatParser = Stat.preFilter((input, i) => c < input.columnNumber(i))
  def stat2(c: Int): MeerkatParser = Stat.preFilter((input, i) => c == input.columnNumber(i))
  
  val For: MeerkatDDParser[Unit] = "for".addColumn ~> Exp ~> ":" ~^ (c => stat1(c).addColumn) ~^ (c => stat2(c).*)
  val If: MeerkatDDParser[Unit] = "if".addColumn ~> Exp ~> ":" ~^ (c => stat1(c).addColumn) ~^ (c => stat2(c).*)
  val Stat: MeerkatParser = "Stat" ::= For | If | 's'

  import util.Configuration._
  
  def main(args: Array[String]): Unit = {
    val input = """for e:
                  |    s 
                  |    if e:
                  |    
                  |        s
                  |        s  
                  |        for e: 
                  |                  s
                  |
                  |                  s
                  |        s
                  |    if    e:   
                  |     s
                  |    s""".stripMargin
    println(input)
    parse(input, Stat, ALL_PARSES, NO_TESTING)
  }
  
}