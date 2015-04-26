/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper

import meerkat._
import scala.util.matching.Regex

/**
 * Code from paper
 * 
 * Statement = LocalVarDecl ``;'' | Assignment ;
 * LocalVarDecl ::= Type Assignment
 * Type ::= int | Identifier
 * Assignment ::= Identifier = Identifier
 *
 * Identifier ::= (a | ... | z)+
 * 
 */

object Statements extends MeerkatParsers {
  
  val Statement: MeerkatParser = 
  "Statement" ::= LocalVarDecl ~~ ";" | Assignment ~~ ";"
  
  val LocalVarDecl: MeerkatParser =
  "LocalVarDecl" ::= Type ~~ Assignment
  
  val Type: MeerkatParser =
  "Type" ::= "int" | Identifier
  
  val Assignment: MeerkatParser =
  "Assignment" ::= Identifier ~~ "=" ~~ Identifier
  
  val Identifier: MeerkatParser = 
  "Identifier" ::= ('a'--'z').+ \ "int" !>> """[a-z]""".r !<< """[a-z]""".r
  
  import util.Configuration._
  
  def main(args: Array[String]) {
    val input: String = "x=in;"
    parse(input, Statement, ALL_PARSES, NO_TESTING)
  }

}