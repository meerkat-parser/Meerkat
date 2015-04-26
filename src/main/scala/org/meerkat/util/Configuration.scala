/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package util

object Configuration extends Enumeration {
  type Configuration = Value
  val FIRST_PARSE, ALL_PARSES, TESTING, NO_TESTING = Value
}