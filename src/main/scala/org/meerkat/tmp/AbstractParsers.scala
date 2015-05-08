package org.meerkat.tmp

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import scala.reflect.ClassTag

trait MonadPlus[+T, M[+F] <: MonadPlus[F,M]] {
  def map[U](f: T => U)(implicit m: Memoizable[T]): M[U]
  def flatMap[U](f: T => M[U])(implicit m: Memoizable[T]): M[U]
  def orElse[U >: T](r: => M[U]): M[U]
  def filter(pred: T => Boolean): M[T]
  
  // Specialization only for optimization purposes
  def _map[U](f: T => U): M[U]
}

trait AbstractParsers {
  
  type Result[+T] <: MonadPlus[T, Result]
  
  protected type Parser[T] = (Input, Int, SPPFLookup) => Result[T]
  
  trait AbstractParser[+T] extends Parser[T] {
    
    private var nm: String = "parser"
    def name: String = nm
    def named(name: String): this.type = { nm = name; this }
    override def toString: String = nm
    
    
    private var hd: AbstractParser[Any] = _
    def head: AbstractParser[Any] = hd
    def headed(head: AbstractParser[Any]): AbstractParser[T] = { hd = head; this }
    
    def isSequence = false
    def isAlternation = false
    
    def isSymbol = false
    def isNonterminal = false
    def isTerminal = false
  }
  
  trait Composable[A, B] {    
    type R
    
    type Sequence <: AbstractParser[R]
    def sequence(f: (Input, Int, SPPFLookup) => Result[R]): Sequence
    
    def index(a: A): Int
    def intermediate(a: A, b: B, p: AbstractParser[R], sppfLookup: SPPFLookup): R    
  }
  
  trait Alternative[A, B >: A] {
    type Alternation <: AbstractParser[B]
    def alternation(f: (Input, Int, SPPFLookup) => Result[B]): Alternation
    
    def result(e: B, p: AbstractParser[B], nt: AbstractParser[Any], sppfLookup: SPPFLookup): B
  }
  
  object AbstractParser {
    
    def parser[T](p: Parser[T]): AbstractParser[T] 
      = new AbstractParser[T] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup) } 
    
    
    /**
     * @param p1 isn't a result of alternation
     * @param p2 is neither a result of alternation nor a result of sequence
     */
    def seq[A: Memoizable, B: Memoizable](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: Composable[A, B]): builder.Sequence = {
      lazy val q: builder.Sequence = builder sequence { 
        (input, i, sppfLookup) => p1(input, i, sppfLookup) flatMap { x1 => p2(input, builder index x1, sppfLookup)._map { x2 => builder intermediate (x1, x2, q, sppfLookup) } } 
      }
      q
    }
  
    /**
     * @param p2 isn't a result of alternation
     */
    def alt[A, B >: A](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: Alternative[A, B], m1: Memoizable[A], m2: Memoizable[B]): builder.Alternation = {
      lazy val q: builder.Alternation = builder alternation {  
        lazy val q1: AbstractParser[A] = if (p1 isAlternation) p1 headed q.head 
                                         else if (p1 isSymbol) parser(p1) 
                                         else p1
        lazy val q2: AbstractParser[B] = if (p2 isSymbol) parser(p2) else p2
        (input, i, sppfLookup) => p1(input, i, sppfLookup).map(x1 => builder result (x1, q1, q.head, sppfLookup)) orElse p2(input, i, sppfLookup).map(x2 => builder result (x2, q2, q.head, sppfLookup)) 
      }
      q
    }
    
    /**
     * @param p isn't a result of alternation
     */
    def alt[B: Memoizable](p: AbstractParser[B])(implicit builder: Alternative[B, B]): AbstractParser[B] = {
      val q = if (p isSymbol) parser(p) else p
      lazy val a: AbstractParser[B] = parser {
        (input, i, sppfLookup) => p(input, i, sppfLookup).map { x => builder result (x, q, a.head, sppfLookup) }
      }
      a
    }
    
  }
  
}

object AbstractCPSParsers extends AbstractParsers {
    
  type Result[+T] = CPSResult[T]
  
  trait CanBecomeNonterminal[A] {
    type Nonterminal <: AbstractParser[A]
    
    def nonterminal(p: (Input, Int, SPPFLookup) => Result[A]): Nonterminal
  }
  
  def memoize[A: Memoizable](p: => AbstractParser[A], name: String)(implicit ntb: CanBecomeNonterminal[A], altb: Alternative[A, A], obj: ClassTag[Result[A]]): ntb.Nonterminal = {
      var table: Array[Result[A]] = null
      lazy val nt: ntb.Nonterminal 
        = ntb nonterminal {
            lazy val q: AbstractParser[A] 
              = (if (p isAlternation) p else AbstractParser.alt(p)) headed nt
        
            (input, i, sppfLookup) => {
              if (table == null) 
                table = new Array(input.length + 1)
              
              val result = table(i)
              if (result == null) {
                table(i) = CPSResult.memo(q(input, i, sppfLookup))
                table(i)
              } else {
                result
              }
            }
          }
      nt named name
  }
  
}