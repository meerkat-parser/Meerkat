package org.meerkat.util

object Configuration extends Enumeration {
  type Configuration = Value
  val FIRST_PARSE, ALL_PARSES, TESTING, NO_TESTING = Value
}