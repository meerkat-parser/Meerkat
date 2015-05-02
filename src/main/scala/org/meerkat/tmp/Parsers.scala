package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode

case class ~[+A, +B](_1: A, _2: B)

trait Parsers {
  
  // TODO: Parameterize over Result[_]
  trait Result[+T] {
    def map[F](f: T => F): Result[F]
    def flatMap[F](f: T => Result[F]): Result[F]
    def orElse[F >: T](r: Result[F]): Result[F]
  }
  
  /**
   *  Abstract parser type with the general semantics of sequence 
   *  and alternation combinators
   */
  trait AbstractParser[+A] extends (Int => Result[A])
  
  protected def seq[A, B, C](f1: Int => Result[A], f2: Int => Result[B])
                  (implicit builder: Composable[A, B, C]): builder.Seq
    = builder sequence { i => f1(i).flatMap { x1 => f2(builder index x1).map { x2 => builder values (x1, x2) } } }
  
  protected def alt[A, B >: A, C](f1: Int => Result[A], f2: Int => Result[B])
                  (implicit builder: Composable[A, B, C]): builder.Alt
    = builder alternation { i => f1(i) orElse f2(i) }
  
  /**
   * Specialized parser types
   */
  trait Recognizer extends AbstractParser[Int] { import Composable.Recognizer; import Recognizer._
    def ~ (r: Recognizer): Sequence = seq(this, r)(Recognizer)
    def | (r: Recognizer): Alternation = alt(this, r)(Recognizer)
  }
  trait DDRecognizer[+T] extends AbstractParser[(Int, T)] { import Composable.DDRecognizer; import DDRecognizer._
    def ~ [F](r: DDRecognizer[F]): Sequence[~[T,F]] = seq(this, r)(ddrecognizer)
    def | [F >: T](r: DDRecognizer[F]): Alternation[F] = alt(this, r)(ddrecognizer)
  }
  
  trait Parser extends AbstractParser[NonPackedNode] { import Composable.Parser; import Parser._
    def ~ (p: Parser): Sequence = seq(this, p)(Parser)
    def | (p: Parser): Alternation = alt(this, p)(Parser)
  }
  trait DDParser[+T] extends AbstractParser[(NonPackedNode, T)] { import Composable.DDParser; import DDParser._
    def ~ [F](p: DDParser[F]): Sequence[~[T,F]] = seq(this, p)(ddparser)
    def | [F >: T](p: DDParser[F]): Alternation[F] = alt(this, p)(ddparser)
  }
  
  trait Composable[A, B, C] {
    type Par <: AbstractParser[C]
    type Seq <: AbstractParser[C]
    type Alt <: AbstractParser[B]
    
    def index(a: A): Int
    def values(a: A, b: B): C
    
    def parser(f: Int => Result[C]): Par
    def sequence(f: Int => Result[C]): Seq
    def alternation(f: Int => Result[B]): Alt
  }
  
  object Composable {
    
    implicit object Recognizer extends Composable[Int, Int, Int] {
      type Par = Recognizer
      type Seq = Sequence
      type Alt = Alternation
      
      trait Sequence extends AbstractParser[Int]
      trait Alternation extends AbstractParser[Int]
      trait Nonterminal extends AbstractParser[Int]
      trait Terminal extends AbstractParser[Int]
      
      def index(a: Int) = a
      def values(a: Int, b: Int) = b
      
      def parser(f: Int => Result[Int]) = new Recognizer { def apply(i: Int) = f(i)}
      def sequence(f: Int => Result[Int]) = new Sequence { def apply(i: Int) = f(i) }
      def alternation(f: Int => Result[Int]) = new Alternation { def apply(i: Int) = f(i) } 
    }
    
    object DDRecognizer {
      trait Sequence[+T] extends AbstractParser[(Int, T)]
      trait Alternation[+T] extends AbstractParser[(Int, T)]
      trait Nonterminal[+T] extends AbstractParser[(Int, T)]
      trait Terminal[+T] extends AbstractParser[(Int, T)]
    
      implicit def ddrecognizer[A, B] = new Composable[(Int, A), (Int, B), (Int, ~[A, B])] {
        type Par = DDRecognizer[~[A,B]]
        type Seq = Sequence[~[A,B]]
        type Alt = Alternation[B]
        
        def index(a: (Int, A)) = a._1
        def values(a: (Int, A), b: (Int, B)): (Int, ~[A,B]) = ??? // intermediate nodes plus values
        
        def parser(f: Int => Result[(Int, ~[A,B])]) = new DDRecognizer[~[A,B]] { def apply(i: Int) = f(i) }
        def sequence(f: Int => Result[(Int, ~[A, B])]) = new Sequence[~[A, B]] { def apply(i: Int) = f(i) }
        def alternation(f: Int => Result[(Int, B)]) = new Alternation[B] { def apply(i: Int) = f(i) } 
      }
    }
    
    implicit object Parser extends Composable[NonPackedNode, NonPackedNode, NonPackedNode] {
      type Par = Parser
      type Seq = Sequence
      type Alt = Alternation
      
      trait Sequence extends AbstractParser[NonPackedNode]
      trait Alternation extends AbstractParser[NonPackedNode]
      trait Nonterminal extends AbstractParser[NonPackedNode]
      trait Terminal extends AbstractParser[NonPackedNode]
      
      def index(a: NonPackedNode) = a.rightExtent
      def values(a: NonPackedNode, b: NonPackedNode): NonPackedNode = ??? // intermediate nodes
      
      def parser(f: Int => Result[NonPackedNode]) = new Parser { def apply(i: Int) = f(i) }
      def sequence(f: Int => Result[NonPackedNode]) = new Sequence { def apply(i: Int) = f(i) }
      def alternation(f: Int => Result[NonPackedNode]) = new Alternation { def apply(i: Int) = f(i) }
    }
    
    object DDParser {
      trait Sequence[+T] extends AbstractParser[(NonPackedNode, T)]
      trait Alternation[+T] extends AbstractParser[(NonPackedNode, T)]
      trait Nonterminal[+T] extends AbstractParser[(NonPackedNode, T)]
      trait Terminal[+T] extends AbstractParser[(NonPackedNode, T)]
    
      implicit def ddparser[A, B] = new Composable[(NonPackedNode, A), (NonPackedNode, B), (NonPackedNode, ~[A, B])] {
        type Par = DDParser[~[A,B]]
        type Seq = Sequence[~[A,B]]
        type Alt = Alternation[B]
        
        def index(a: (NonPackedNode, A)) = a._1.rightExtent
        def values(a: (NonPackedNode, A), b: (NonPackedNode, B)): (NonPackedNode, ~[A,B]) = ??? // intermediate nodes plus values
        
        def parser(f: Int => Result[(NonPackedNode, ~[A,B])]) = new DDParser[~[A,B]] { def apply(i: Int) = f(i) }
        def sequence(f: Int => Result[(NonPackedNode, ~[A, B])]) = new Sequence[~[A, B]] { def apply(i: Int) = f(i) }
        def alternation(f: Int => Result[(NonPackedNode, B)]) = new Alternation[B] { def apply(i: Int) = f(i) } 
      }
    
    }
  }
  
  
}
