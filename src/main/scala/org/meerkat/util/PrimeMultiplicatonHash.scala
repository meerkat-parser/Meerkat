/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package util

object PrimeMultiplicatonHash {
  
  def hashCode(k1: Int, k2: Int): Int = {
     var result: Int = 17
     result = 31 * result + k1
     result = 31 * result + k2
	 return result
  }
  
  def hashCode(k1: Int, k2: Int, k3: Int): Int = {
     var result: Int = 17
     result = 31 * result + k1
     result = 31 * result + k2
     result = 31 * result + k3
	 return result
  }  

}