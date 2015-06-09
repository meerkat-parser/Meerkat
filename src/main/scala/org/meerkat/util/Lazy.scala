package org.meerkat.util

class Lazy[T](v: => T) {
  var _value: Option[T] = None
  def value: T = _value.getOrElse({ _value = Option(v); _value.get })
}