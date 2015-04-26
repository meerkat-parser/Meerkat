/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package util

class Lazy[T](v: => T) {
  var _value: Option[T] = None
  def value: T = _value.getOrElse({ _value = Option(v); _value.get })
}