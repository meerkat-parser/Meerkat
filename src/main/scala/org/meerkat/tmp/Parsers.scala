package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode

trait Parsers {
  
  trait Result[+T] {
    def map[F](f: T => F): Result[F]
    def flatMap[F](f: T => Result[F]): Result[F]
  }
  
  val l = List()
  
  trait AbstractParser[+A] extends (Int => Result[A]) 
  
  trait Recognizer extends AbstractParser[Int]
  trait DDRecognizer[+T] extends AbstractParser[(Int, T)]
  
  trait Parser extends AbstractParser[NonPackedNode]
  trait DDParser[+T] extends AbstractParser[(NonPackedNode, T)]
  
  def seq[A, B, C, That <: AbstractParser[C]](f1: Int => Result[A], f2: Int => Result[B])
                                             (implicit builder: Composable[A, B, C, That]): Sequence[C]
    = builder sequence { (i: Int) => f1(i).flatMap { x1 => f2(builder index x1).map { x2 => builder values (x1, x2) } } }
    
  case class ~[A, B]()
  
  trait Composable[A, B, C, R] {
    def index(a: A): Int
    def values(a: A, b: B): C
    
    def parser(f: Int => Result[C]): R
    def sequence(f: Int => Result[C]): Sequence[C] = new Sequence[C] { def apply(i: Int) = f(i) }
    def alternation(f: Int => Result[C]): Alternation[C] = new Alternation[C] { def apply(i: Int) = f(i) }
  }
  
  object Composable {
    implicit object Rec extends Composable[Int, Int, Int, Recognizer] {
      def index(a: Int) = a
      def values(a: Int, b: Int) = b
      def parser(f: Int => Result[Int]) = new Recognizer { def apply(i: Int) = f(i)}
    }
  
    implicit object Par extends Composable[NonPackedNode, NonPackedNode, NonPackedNode, Parser] {
      def index(a: NonPackedNode) = a.rightExtent
      def values(a: NonPackedNode, b: NonPackedNode): NonPackedNode = ??? // intermediate nodes
      def parser(f: Int => Result[NonPackedNode]): Parser = ???
    }
  
    implicit def ddpar[A, B] = new Composable[(NonPackedNode, A), (NonPackedNode, B), (NonPackedNode, ~[A, B]), DDParser[_]] {
      def index(a: (NonPackedNode, A)) = a._1.rightExtent
      def values(a: (NonPackedNode, A), b: (NonPackedNode, B)): (NonPackedNode, ~[A,B]) = ???
      def parser(f: Int => Result[(NonPackedNode, ~[A,B])]): DDParser[(NonPackedNode, ~[A, B])] = ???
    }
  }
  
  
        
  trait Sequence[+T] extends AbstractParser[T]    
  trait Alternation[+T] extends AbstractParser[T]
  
  trait Nonterminal extends Parser
  trait Terminal extends Parser

//  protected implicit def parser(f: Int => Result[NonPackedNode]): Parser = ???
//  protected implicit def sequence(f: Int => Result[NonPackedNode]): Sequence = ???
//  protected implicit def alternation(f: Int => Result[NonPackedNode]): Alternation = ???
  
}

object Parsers {
  
}