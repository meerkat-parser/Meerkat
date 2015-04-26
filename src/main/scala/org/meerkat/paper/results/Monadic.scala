/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper.results

import scala.language.higherKinds

object Fix {
  def fix[A, B](f: (A=>B)=>(A=>B)): A=>B = { lazy val p: A=>B = f(t => p(t)); p }
}

trait MonadPlus[T, M[_] <: MonadPlus[_, M]] {
  def map[U](f: T => U): M[U]
  def flatMap[U](f: T => M[U]): M[U]
  def orElse(r: => M[T]): M[T]
}