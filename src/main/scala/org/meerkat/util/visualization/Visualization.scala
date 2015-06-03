package org.meerkat.util.visualization

object Shape extends Enumeration {

  type Shape = Value

  val Rectangle = Shape("shape = box, ")
  val Diamond = Shape("shape = diamond, ")
  val Circle = Shape("shape = circle,")

  def apply(s: String): Shape = Value(s)
}

object Style extends Enumeration {
  type Style = Value

  val Default = Style("")
  val Rounded = Style("style = rounded, ")

  def apply(s: String): Style = Value(s)
}

object Color extends Enumeration {
  type Color = Value;

  val Black   = Color("black")
  val Red     = Color("red")
  
  def apply(s: String): Color = Value(s)
}