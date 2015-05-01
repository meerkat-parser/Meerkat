package org.meerkat.util

trait Key

case class IntKey2(k1: Int, k2: Int, h: (Int, Int) => Int) extends Key {
  val hash = h(k1, k2)
  
  override def hashCode = hash
  
  override def equals(o: Any) = o match {
    case IntKey2(k1, k2, h) => this.k1 == k1 && this.k2 == k2
    case _                  => false
  } 
}

case class IntKey3(k1: Int, k2: Int, k3: Int, h: (Int, Int, Int) => Int) extends Key {
  val hash = h(k1, k2, k3)
  
  override def hashCode = hash
  
  override def equals(o: Any) = o match {
    case IntKey3(k1, k2, k3, h) => this.k1 == k1 && this.k2 == k2 && this.k3 == k3
    case _                      => false
  }
}