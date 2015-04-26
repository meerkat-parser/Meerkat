/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package paper.combinators

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import paper.results.CPSResults

trait CPSRecognizers extends Recognizers with CPSResults

trait MemoizedCPSRecognizers extends Recognizers with MemoizedCPS {
  override def rule(nt: String, alts: Recognizer*) = memo(super.rule(nt, alts:_*))
}