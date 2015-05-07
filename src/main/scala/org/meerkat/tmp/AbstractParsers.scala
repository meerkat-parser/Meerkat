package org.meerkat.tmp

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input

trait MonadPlus[+T, M[_] <: MonadPlus[_,M]] {
  def map[U](f: T => U): M[U]
  def flatMap[U](f: T => M[U]): M[U]
  def orElse[U >: T](r: => M[U]): M[U]
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
    type Alternation <: AbstractParser[B]
    
    def index(a: A): Int
    def intermediate(a: A, b: B, p: AbstractParser[R]): R
    def nonterminal(elem: B, p: AbstractParser[B], nt: AbstractParser[Any]): B
    
    def sequence(f: (Input, Int, SPPFLookup) => Result[R]): Sequence
    def alternation(f: (Input, Int, SPPFLookup) => Result[B]): Alternation    
  }
  
  object AbstractParser {
    
    def parser[T](p: Parser[T]): AbstractParser[T] 
      = new AbstractParser[T] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup) } 
    
    protected def seq[A, B](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: Composable[A, B]): builder.Sequence = {
      lazy val q: builder.Sequence = builder sequence { 
        (input, i, sppfLookup) => p1(input, i, sppfLookup) flatMap { x1 => p2(input, builder index x1, sppfLookup).map { x2 => builder intermediate (x1, x2, q) } } 
      }
      q
    }
  
    protected def alt[A, B >: A](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: Composable[A, B]): builder.Alternation = {
      lazy val q: builder.Alternation = builder alternation { 
        lazy val q1: AbstractParser[A] = if (p1 isNonterminal) parser(p1) else p1 passHead q.head
        lazy val q2: AbstractParser[B] = if (p2 isNonterminal) parser(p2) else p2
        (input, i, sppfLookup) => p1(input, i, sppfLookup).map(x1 => builder nonterminal (x1, q1, q.head)) orElse p2(input, i, sppfLookup).map(x2 => builder nonterminal (x2, q2, q.head)) 
      }
      q
    } 
  }
  
}