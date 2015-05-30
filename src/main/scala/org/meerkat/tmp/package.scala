package org.meerkat

/**
 * @author Anastasia Izmaylova
 */
package object tmp {
  
  case class ~[+A,+B](_1: A, _2: B)
  
  trait <:<[A,B]
  implicit def sub[A,B >: A]: A <:< B = null
  
  trait <:!<[A,B]
  implicit def nsub[A,B]: A <:!< B = null
  implicit def nsubAmb1[A,B >: A]: A <:!< B = null
  implicit def nsubAmb2[A,B >: A]: A <:!< B = null

  type ![T] = { type f[U] = U <:!< T }
  
  sealed trait NoValue
  
  trait |~|[A,B] { type R }
  implicit def f1[A <: NoValue,B <: NoValue] = new |~|[NoValue,NoValue] { type R = NoValue }
  implicit def f2[A <: NoValue,B: ![NoValue]#f] = new |~|[NoValue,B] { type R = B }
  implicit def f3[A: ![NoValue]#f,B <: NoValue] = new |~|[A,NoValue] { type R = A }
  implicit def f4[A: ![NoValue]#f,B: ![NoValue]#f] = new |~|[A,B] { type R = (A,B) }
  
}