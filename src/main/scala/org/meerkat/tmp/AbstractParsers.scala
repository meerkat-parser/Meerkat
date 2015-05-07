package org.meerkat.tmp

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input

trait MonadPlus[+T, M[+F] <: MonadPlus[F,M]] {
  def map[U](f: T => U)(implicit m: Memoizable[T]): M[U]
  def flatMap[U](f: T => M[U])(implicit m: Memoizable[T]): M[U]
  def orElse[U >: T](r: => M[U]): M[U]
  def filter(pred: T => Boolean): M[T]
  
  // Optimization
  def _map[U](f: T => U): M[U]
}

trait AbstractParsers {
  
  type Result[+T] <: MonadPlus[T, Result]
  
  protected type Parser[T] = (Input, Int, SPPFLookup) => Result[T]
  
  trait AbstractParser[+T] extends Parser[T] {
    
    private var hd: AbstractParser[Any] = _
    protected def head: AbstractParser[Any] = hd
    protected def passHead(head: AbstractParser[Any]): AbstractParser[T] = { hd = head; this }
    
    protected def isSequence = false
    protected def isAlternation = false
    protected def isNonterminal = false
    protected def isTerminal = false
  }
  
  trait Composable[A, B] {    
    type R
    
    type Sequence <: AbstractParser[R]
    def sequence(f: (Input, Int, SPPFLookup) => Result[R]): Sequence
    
    def index(a: A): Int
    def intermediate(a: A, b: B, p: AbstractParser[R]): R    
  }
  
  trait Alternative[A, B >: A] {
    type Alternation <: AbstractParser[B]
    def alternation(f: (Input, Int, SPPFLookup) => Result[B]): Alternation
    
    def result(elem: B, p: AbstractParser[B], nt: AbstractParser[Any]): B
  }
  
  object AbstractParser {
    
    def parser[T](p: Parser[T]): AbstractParser[T] 
      = new AbstractParser[T] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup) } 
    
    protected def seq[A: Memoizable, B: Memoizable](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: Composable[A, B]): builder.Sequence = {
      lazy val q: builder.Sequence = builder sequence { 
        (input, i, sppfLookup) => p1(input, i, sppfLookup) flatMap { x1 => p2(input, builder index x1, sppfLookup)._map { x2 => builder intermediate (x1, x2, q) } } 
      }
      q
    }
  
    protected def alt[A, B >: A](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: Alternative[A, B], m1: Memoizable[A], m2: Memoizable[B]): builder.Alternation = {
      lazy val q: builder.Alternation = builder alternation { 
        lazy val q1: AbstractParser[A] = if (p1 isNonterminal) parser(p1) else p1 passHead q.head
        lazy val q2: AbstractParser[B] = if (p2 isNonterminal) parser(p2) else p2
        (input, i, sppfLookup) => p1(input, i, sppfLookup).map(x1 => builder result (x1, q1, q.head)) orElse p2(input, i, sppfLookup).map(x2 => builder result (x2, q2, q.head)) 
      }
      q
    } 
  }
  
}