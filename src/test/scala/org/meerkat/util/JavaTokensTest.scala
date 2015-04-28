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
  
  test("DecimialIntegerLiteral") {
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
  
  test("HexIntegerLiteral") {
    assert(HexIntegerLiteral.matcher.matches("0x0"))
    assert(HexIntegerLiteral.matcher.matches("0x1a0f"))
    assert(HexIntegerLiteral.matcher.matches("0x1_a0f_aL"))
    assert(HexIntegerLiteral.matcher.matches("0x1_a0_f_ff_a"))
    assert(!HexIntegerLiteral.matcher.matches("0x"))
    assert(!HexIntegerLiteral.matcher.matches("0x_1"))
  }
  
  test("OctalIntegerLiteral") {
    assert(OctalIntegerLiteral.matcher.matches("01"))
    assert(OctalIntegerLiteral.matcher.matches("0_1"))
    assert(OctalIntegerLiteral.matcher.matches("0___1"))
    assert(OctalIntegerLiteral.matcher.matches("0_1__2"))
    assert(OctalIntegerLiteral.matcher.matches("0_1__2L"))
    assert(OctalIntegerLiteral.matcher.matches("0_1__2l"))
    assert(OctalIntegerLiteral.matcher.matches("0123456"))
    assert(!OctalIntegerLiteral.matcher.matches("0")) 
  }
  
  test("BinaryIntegerLiteral") {
    assert(BinaryIntegerLiteral.matcher.matches("0b0"))
    assert(BinaryIntegerLiteral.matcher.matches("0b011"))
    assert(BinaryIntegerLiteral.matcher.matches("0b011"))
    assert(BinaryIntegerLiteral.matcher.matches("0b0_11____01"))
    assert(BinaryIntegerLiteral.matcher.matches("0b0_11____01L"))
    assert(!BinaryIntegerLiteral.matcher.matches("0"))
    assert(!BinaryIntegerLiteral.matcher.matches("0b"))
    assert(!BinaryIntegerLiteral.matcher.matches("0_b"))
  }
  
  test("DecimalFloatingPointLiteral") {
    assert(DecimalFloatingPointLiteral.matcher.matches("1e1f"))
    assert(DecimalFloatingPointLiteral.matcher.matches("2.f"))
    assert(DecimalFloatingPointLiteral.matcher.matches(".3f"))
    assert(DecimalFloatingPointLiteral.matcher.matches("0f"))
    assert(DecimalFloatingPointLiteral.matcher.matches("3.14f"))
    assert(DecimalFloatingPointLiteral.matcher.matches("6.022137e+23f"))
    assert(DecimalFloatingPointLiteral.matcher.matches("1e1"))
    assert(DecimalFloatingPointLiteral.matcher.matches("2."))
    assert(DecimalFloatingPointLiteral.matcher.matches(".3"))
    assert(DecimalFloatingPointLiteral.matcher.matches("0.0"))
    assert(DecimalFloatingPointLiteral.matcher.matches("3.14"))
    assert(DecimalFloatingPointLiteral.matcher.matches("1e-9d"))
    assert(DecimalFloatingPointLiteral.matcher.matches("1e137"))
  }
  
  test("BinaryFloatingPointLiteral") {
    assert(HexadecimalFloatingPointLiteral.matcher.matches("0x1.8p1"))
    assert(HexadecimalFloatingPointLiteral.matcher.matches("0x1.999999999999ap-4"))
    assert(HexadecimalFloatingPointLiteral.matcher.matches("0x1.99____9999___9999_99ap-4"))
  }
  
  test("CharacterLiteral") {
    assert(CharacterLiteral.matcher.matches("'a'"))
    assert(CharacterLiteral.matcher.matches("'%'"))
    assert(CharacterLiteral.matcher.matches("'\t'"))
    assert(CharacterLiteral.matcher.matches("'\\\\'"))
    assert(CharacterLiteral.matcher.matches("'\\''"))
    assert(CharacterLiteral.matcher.matches("'\\177'"))
    assert(CharacterLiteral.matcher.matches("'\\u03a9'"))
    assert(CharacterLiteral.matcher.matches("'\\uFFFF'"))
    assert(CharacterLiteral.matcher.matches("'#'")) 
  }
  
  test("StringLiteral") {
    assert(StringLiteral.matcher.matches("\"\""))
    assert(StringLiteral.matcher.matches("\"\\\"\""))
    assert(StringLiteral.matcher.matches("\"This is a string\""))
    assert(StringLiteral.matcher.matches("\"\\141\""))
    assert(StringLiteral.matcher.matches("\"\\57\""))
    assert(StringLiteral.matcher.matches("\"\\6\""))
  }
  
  test("Comment") {
    assert(Comment.matcher.matches("//This is a simple comment"))
    assert(Comment.matcher.matches("/* aaaa */"))
    assert(Comment.matcher.matches("/* this comment /* // /** ends here: */"))
  }
  
  test("Keyword") {
    assert(Keyword.matcher.matches("null"))
    assert(Keyword.matcher.matches("abstract"))
    assert(Keyword.matcher.matches("strictfp"))
    assert(Keyword.matcher.matches("throws"))
    assert(Keyword.matcher.matches("int"))
    assert(Keyword.matcher.matches("super"))
  }
  
}