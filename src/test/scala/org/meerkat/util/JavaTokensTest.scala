package org.meerkat.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import util.Input
import util.JavaTokens2._

@RunWith(classOf[JUnitRunner])
class JavaTokensTest extends FunSuite {

  test("Identifier") {
    Identifier.matches(Input("identifier"))
    Identifier.matches(Input("_"))
    Identifier.matches(Input("_Xyx"))
    Identifier.matches(Input("x_y_z_123"))
    Identifier.matches(Input("$_1_123"))
    Identifier.matches(Input("1definer"))
  }
  
  test("Decimial Integer Number") {
    
  }
  
}