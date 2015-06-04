package org.meerkat.tmp

/**
 * @author Anastasia Izmaylova
 */

import org.meerkat.Syntax._
import org.meerkat.tmp._
import DDParsers._
import Parsers._
import DefaultLayout._

object DDTest {
  
  object Test1 {
    
    val Num = syn { "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" }
    
    val E: DataNonterminal[String] = syn { ( E ~ "+" ~ E ).map { case x~y => s"($x plus $y)" } | Num.map { s => s } }
    
    val Octet = syn { """.""".r }
    
    val L8 = syn { "~{" ~ Num.map { s => s.toInt } ~"}" ~>> { n => println("num:" + n); Octets(n,Octet) } }
    
    def Octets(n: Int, p: Nonterminal) 
      = n match {
          case 0 => syn { Ø }
          case 1 => syn { p }
          case _ => syn { (1 to n-2).foldLeft(p ~ p)((q,_) => q ~ p) }
        }
    
    def main(args: Array[String]): Unit = {
      parse("~{ 9 } X X X X X X X X X",L8)
    }     
    
  }
  
  def main(args: Array[String]): Unit = {
    Test1.main(args)
  }
  
}