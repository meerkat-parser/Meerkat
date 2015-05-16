package org.meerkat.tmp

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input
import scala.reflect.ClassTag
import org.meerkat.sppf.Slot

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
  
  trait AbstractParser[+T] extends Parser[T] with Slot {
    
    def ruleType = ???
    
    def name: String = ""
    override def toString: String = name
    
    
    def head: Option[AbstractParser[Any]] = None
    def pass(head: AbstractParser[Any]): Unit = {}
    
    def isSequence = false
    def isAlternation = false
    
    def isSymbol = false
    def isNonterminal = false
    def isTerminal = false
  }
  
  trait CanBuildSequence[A, B] {    
    type R
    
    type Sequence <: AbstractParser[R]
    def sequence(f: (Input, Int, SPPFLookup) => Result[R]): Sequence
    
    def index(a: A): Int
    def intermediate(a: A, b: B, p: AbstractParser[R], sppfLookup: SPPFLookup): R    
  }
  
  trait CanBuildAlternation[B] {
    type Alternation <: AbstractParser[B]
    def alternation(f: AbstractParser[Any] => ((Input, Int, SPPFLookup) => Result[B])): Alternation
    
    def result(e: B, p: AbstractParser[B], nt: AbstractParser[Any], sppfLookup: SPPFLookup): B
  }
  
  object AbstractParser {
    
    def parser[T](p: Parser[T]): AbstractParser[T] 
      = new AbstractParser[T] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup) }   
    
    /**
     * @param p1 isn't a result of alternation
     * @param p2 is neither a result of alternation nor a result of sequence
     */
    def seq[A: Memoizable, B: Memoizable](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder: CanBuildSequence[A, B]): builder.Sequence = {
      lazy val q: builder.Sequence = builder sequence { 
        (input, i, sppfLookup) => p1(input, i, sppfLookup) flatMap { x1 => p2(input, builder index x1, sppfLookup)._map { x2 => builder intermediate (x1, x2, q, sppfLookup) } } 
      }
      q
    }
  
    /**
     * @param p2 isn't a result of alternation
     */
    def alt[A, B >: A](p1: AbstractParser[A], p2: AbstractParser[B])(implicit builder1: CanBuildAlternation[A], builder2: CanBuildAlternation[B], m1: Memoizable[A], m2: Memoizable[B]): builder2.Alternation = {
      builder2 alternation { head => 
        val q1 = if (p1 isAlternation) p1 else alt(p1); q1 pass head
        val q2 = alt(p2); q2 pass head
        (input, i, sppfLookup) => q1(input, i, sppfLookup) orElse q2(input, i, sppfLookup) 
      } 
    }
    
    /**
     * @param p isn't a result of alternation
     */
    def alt[B: Memoizable](p: AbstractParser[B])(implicit builder: CanBuildAlternation[B]): AbstractParser[B] = {
      lazy val q: AbstractParser[B] = builder alternation { head =>
        (input, i, sppfLookup) => p(input, i, sppfLookup).map { x => builder result (x, q, head, sppfLookup) }
      }
      q
    }
    
  }
  
}

object AbstractCPSParsers extends AbstractParsers {
    
  type Result[+T] = CPSResult[T]
  
  trait CanBuildNonterminal[A] {
    type Nonterminal <: AbstractParser[A]
    
    def nonterminal(name: String, p: AbstractParser[Any] => (Input, Int, SPPFLookup) => Result[A]): Nonterminal
  }
  
  import AbstractParser._
  
  def memoize[A: Memoizable](p: => AbstractParser[A], name: String)(implicit ntb: CanBuildNonterminal[A], altb: CanBuildAlternation[A], obj: ClassTag[Result[A]]): ntb.Nonterminal = {
    var table: Array[Result[A]] = null
    
    ntb.nonterminal(name, head => {
      lazy val parser: AbstractParser[A] = { var q = p; if (!q.isAlternation) q = alt(q); q pass head; q }
        
      (input, i, sppfLookup) => {
        if (table == null) table = new Array(input.length + 1)
              
        val result = table(i)
        if (result == null) {
          table(i) = CPSResult.memo(parser(input, i, sppfLookup)); table(i)
        } else {
          result
        }
      }
    })
  }
  
}