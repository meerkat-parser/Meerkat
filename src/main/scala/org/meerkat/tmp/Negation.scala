package org.meerkat.tmp

object Negation {
  
  trait <:!<[A, B]

  implicit def nsub[A, B] : A <:!< B = null
  implicit def nsubAmbig1[A, B >: A] : A <:!< B = null
  implicit def nsubAmbig2[A, B >: A] : A <:!< B = null

  type |!|[T] = { type f[U] = U <:!< T }

}