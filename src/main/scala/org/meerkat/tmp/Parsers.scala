package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode
import org.meerkat.tmp.Negation._

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
  trait AbstractParser[+A] extends (Int => Result[A]) {
    def isSequence = false
    def isAlternation = false
  }
  
  trait Composable[A, B] {
    type R
    
    type Par <: AbstractParser[R]
    type Seq <: AbstractParser[R]
    type Alt <: AbstractParser[B]
    
    def index(a: A): Int
    def values(a: A, b: B): R
    
    def parser(f: Int => Result[R]): Par
    def sequence(f: Int => Result[R]): Seq
    def alternation(f: Int => Result[B]): Alt
  }
  
  protected def seq[A, B](f1: Int => Result[A], f2: Int => Result[B])
                         (implicit builder: Composable[A, B]): builder.Seq
    = builder sequence { i => f1(i) flatMap { x1 => f2(builder index x1).map { x2 => builder values (x1, x2) } } }
  
  protected def alt[A, B >: A](f1: Int => Result[A], f2: Int => Result[B])
                              (implicit builder: Composable[A, B]): builder.Alt
    = builder alternation { i => f1(i) orElse f2(i) }
  
  /**
   * Specialized parser types
   */
  trait Recognizer extends AbstractParser[Int] { import Composable.Recognizer; import Recognizer._
    def ~ (r: Recognizer): Sequence = seq(this, r)
    def | (r: Recognizer): Alternation = alt(this, r)
  }
  trait DDRecognizer[+T] extends AbstractParser[(Int, T)] { import Composable.DDRecognizer; import DDRecognizer._
    def ~ [F](r: DDRecognizer[F]): Sequence[~[T,F]] = seq(this, r)(ddrecognizer)
    def | [F >: T](r: DDRecognizer[F]): Alternation[F] = alt(this, r)(ddrecognizer)
  }
  
  trait Parser extends AbstractParser[NonPackedNode] { import Composable.Parser; import Parser._
    def ~ (p: Parser): Sequence = seq(this, p)
    def | (p: Parser): Alternation = alt(this, p)
  }
  
  trait DDParser[+T] extends AbstractParser[(NonPackedNode, T)] { import Composable.DDParser; import DDParser._
    def ~ [F](p: DDParser[F]): Sequence[~[T,F]] = seq(this, p)(ddparser)
    def | [F >: T](p: DDParser[F]): Alternation[F] = alt(this, p)(ddparser)
  }
  
  type Prec = Int
  
  trait OperatorParser extends (Prec => Parser) {
    def ~ (p: OperatorParser): OperatorParser = ???
    def |>| (p: OperatorParser): OperatorParser = ???
  }
  
  implicit class ParserOps(q: Parser) { import Composable.DDParser; import DDParser._
    def ~ [F](p: DDParser[F]): Sequence[F] = seq(q, p)(ddparser2)
    def ~ (p: OperatorParser): OperatorParser = ???
  }
  
  implicit class DDParserOps[+T](q: DDParser[T]) { import Composable.DDParser; import DDParser._
    def ~ (p: Parser): Sequence[T] = seq(q, p)(ddparser1)
  }
  
  implicit class OperatorParserOps(q: OperatorParser) {
    def ~ (p: Parser): OperatorParser = ???
  }
  
  object Composable {
    
    implicit object Recognizer extends Composable[Int, Int] {
      type R = Int
      
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
      
      implicit def ddrecognizer[A, B] = new Composable[(Int, A), (Int, B)] {
        type R = (Int, ~[A, B])
        
        type Par = DDRecognizer[~[A,B]]
        type Seq = Sequence[~[A, B]]
        type Alt = Alternation[B]
        
        def index(a: (Int, A)) = a._1
        def values(a: (Int, A), b: (Int, B)): (Int, ~[A,B]) = ??? // intermediate nodes plus values
        
        def parser(f: Int => Result[(Int, ~[A,B])]) = new DDRecognizer[~[A,B]] { def apply(i: Int) = f(i) }
        def sequence(f: Int => Result[(Int, ~[A, B])]) = new Sequence[~[A, B]] { def apply(i: Int) = f(i) }
        def alternation(f: Int => Result[(Int, B)]) = new Alternation[B] { def apply(i: Int) = f(i) }
      }
    }
    
    implicit object Parser extends Composable[NonPackedNode, NonPackedNode] {
      type R = NonPackedNode
      
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
    
      implicit def ddparser[A, B] = new Composable[(NonPackedNode, A), (NonPackedNode, B)] {
        type R = (NonPackedNode, ~[A, B])
        
        type Par = DDParser[~[A,B]]
        type Seq = Sequence[~[A,B]]
        type Alt = Alternation[B]
        
        def index(a: (NonPackedNode, A)) = a._1.rightExtent
        def values(a: (NonPackedNode, A), b: (NonPackedNode, B)): (NonPackedNode, ~[A,B]) = ??? // intermediate nodes plus values
        
        def parser(f: Int => Result[(NonPackedNode, ~[A,B])]) = new DDParser[~[A,B]] { def apply(i: Int) = f(i) }
        def sequence(f: Int => Result[(NonPackedNode, ~[A, B])]) = new Sequence[~[A, B]] { def apply(i: Int) = f(i) }
        def alternation(f: Int => Result[(NonPackedNode, B)]) = new Alternation[B] { def apply(i: Int) = f(i) } 
      }
      
      implicit def ddparser1[A] = new Composable[(NonPackedNode, A), NonPackedNode] {
        type R = (NonPackedNode, A)
        
        type Par = DDParser[A]
        type Seq = Sequence[A]
        type Alt = Nothing
        
        def index(a: (NonPackedNode, A)) = a._1.rightExtent
        def values(a: (NonPackedNode, A), b: NonPackedNode): (NonPackedNode, A) = ??? // intermediate nodes plus values
        
        def parser(f: Int => Result[(NonPackedNode, A)]) = new DDParser[A] { def apply(i: Int) = f(i) }
        def sequence(f: Int => Result[(NonPackedNode, A)]) = new Sequence[A] { def apply(i: Int) = f(i) }
        def alternation(f: Int => Result[NonPackedNode]) = ??? 
      }
      
      implicit def ddparser2[B] = new Composable[NonPackedNode, (NonPackedNode, B)] {
        type R = (NonPackedNode, B)
        
        type Par = DDParser[B]
        type Seq = Sequence[B]
        type Alt = Alternation[B]
        
        def index(a: NonPackedNode) = a.rightExtent
        def values(a: NonPackedNode, b: (NonPackedNode, B)): (NonPackedNode, B) = ??? // intermediate nodes plus values
        
        def parser(f: Int => Result[(NonPackedNode, B)]) = new DDParser[B] { def apply(i: Int) = f(i) }
        def sequence(f: Int => Result[(NonPackedNode, B)]) = new Sequence[B] { def apply(i: Int) = f(i) }
        def alternation(f: Int => Result[(NonPackedNode, B)]) = ???
      }
    }
    
  }
  
  
}
