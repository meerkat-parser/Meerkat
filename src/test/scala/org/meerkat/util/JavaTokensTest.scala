package org.meerkat.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import util.Input
import util.JavaTokens2._

@RunWith(classOf[JUnitRunner])
class JavaTokensTest extends FunSuite {

  test("Identifier") {
    assert(Identifier.matches(Input("identifier")))
    assert(Identifier.matches(Input("_")))
    assert(Identifier.matches(Input("_Xyx")))
    assert(Identifier.matches(Input("x_y_z_123")))
    assert(Identifier.matches(Input("$_1_123")))
    assert(!Identifier.matches(Input("1definer")))
  }
  
  test("Decimial Integer Number") {
    
  }
  
}