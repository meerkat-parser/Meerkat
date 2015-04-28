package org.meerkat.util

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import util.Input
import util.JavaTokens2._

@RunWith(classOf[JUnitRunner])
class JavaTokensTest extends FunSuite {

  test("Identifier") {
    assert(Identifier.matcher.matches("identifier"))
    assert(Identifier.matcher.matches("_"))
    assert(Identifier.matcher.matches("_Xyx"))
    assert(Identifier.matcher.matches("x_y_z_123"))
    assert(Identifier.matcher.matches("$_1_123"))
    assert(!Identifier.matcher.matches("1definer"))
  }
  
  test("Decimial Integer Number") {
    assert(DecimalIntegerLiteral.matcher.matches("0"))
    assert(DecimalIntegerLiteral.matcher.matches("1"))
    assert(DecimalIntegerLiteral.matcher.matches("10"))
    assert(DecimalIntegerLiteral.matcher.matches("234000"))
    assert(DecimalIntegerLiteral.matcher.matches("234000L"))
    assert(DecimalIntegerLiteral.matcher.matches("0"))
    assert(DecimalIntegerLiteral.matcher.matches("23_4___00_0l"))
    assert(DecimalIntegerLiteral.matcher.matches("23_4___00_0L"))
    assert(!DecimalIntegerLiteral.matcher.matches("_123"))
    assert(!DecimalIntegerLiteral.matcher.matches("0123"))
  }
  
}