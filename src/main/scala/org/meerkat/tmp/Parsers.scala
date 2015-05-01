package org.meerkat.tmp

import org.meerkat.sppf.NonPackedNode

trait Parsers {
  
  type Result[+T]
  
  val l = List()
  
  trait AbstractParser[+A] extends (Int => Result[A]) 
  
  trait Recognizer extends AbstractParser[Int]
  trait DDRecognizer[+T] extends AbstractParser[(Int, T)]
  
  trait Parser extends AbstractParser[NonPackedNode]
  trait DDParser[+T] extends AbstractParser[(NonPackedNode, T)]
  
  def seq[A, B, That](f1: Int => Result[A], f2: Int => Result[B])
        (implicit composer: Composable[A, B, That]): That
    = ???
    
  case class ~[A, B]()
  
  trait Composable[A, B, R] {
    type T
    def build(f: Int => Result[T]): R
  }
  
  object Composable {
    implicit object rec extends Composable[Int, Int, Recognizer] {
      type T = Int
      def build(f: Int => Result[Int]): Recognizer = ???
    }
  
    implicit object par extends Composable[NonPackedNode, NonPackedNode, Parser] {
      type T = NonPackedNode
      def build(f: Int => Result[NonPackedNode]): Parser = ???
    }
  
    implicit def ddpar[A, B] = new Composable[(NonPackedNode, A), (NonPackedNode, B), DDParser[_]] {
      type T = (NonPackedNode, ~[A,B])
      def build(f: Int => Result[(NonPackedNode, ~[A,B])]): DDParser[(NonPackedNode, ~[A, B])] = ???
    }  
  }
  
  
        
  trait Sequence[+T] extends AbstractParser[T]
  
  implicit def sequence[A](f: Int => Result[A]): Sequence[A]
    = new Sequence[A] { def apply(i: Int) = f(i) }
  
  trait Alternation[+T] extends AbstractParser[T]
  
  implicit def alternation[A](f: Int => Result[A]): Alternation[A] 
    = new Alternation[A] { def apply(i: Int) = f(i) }
  
  trait Nonterminal extends Parser
  trait Terminal extends Parser

//  protected implicit def parser(f: Int => Result[NonPackedNode]): Parser = ???
//  protected implicit def sequence(f: Int => Result[NonPackedNode]): Sequence = ???
//  protected implicit def alternation(f: Int => Result[NonPackedNode]): Alternation = ???
  
}

object Parsers {
  
}