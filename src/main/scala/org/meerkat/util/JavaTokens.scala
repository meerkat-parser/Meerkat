/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package util

import scala.util.matching.Regex
import java.util.regex.Pattern

object JavaTokens {

  /**
   * An identifier is an unlimited-length sequence of Java letters and Java digits, 
   * the first of which must be a Java letter.
   */
  val Identifier:Regex = """[a-zA-Z_$]([a-zA-Z_$]|[0-9])*""".r
  
  /**
   * A decimal number is either the number 0, or a non-zero
   * number optionally followed by a number of numbers, interspersed by
   * underscore:
   * 
   */
  val DecimalIntegerLiteral:Regex = """(0|[1-9]([0-9]([0-9_]*[0-9])?)?|[1-9][_]+[0-9]([0-9_]*[0-9])?)[lL]?""".r
  
  
  /**
   * A hexadecimal numeral consists of the leading ASCII characters
   * 0x or 0X followed by one or more ASCII hexadecimal digits interspersed with underscores,
   * 
   */
  val HexIntegerLiteral:Regex = """0[xX][0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?[lL]?""".r
  
  
  /**
   * An octal numeral consists of an ASCII digit 0 followed by one or more of the 
   * ASCII digits 0 through 7 interspersed with underscores
   */
  val OctalIntegerLiteral:Regex = """[0][_]*[0-7]([0-7_]*[0-7])?[lL]?""".r
  
  
  /**
   * A binary numeral consists of the leading ASCII characters 0b or 0B followed 
   * by one or more of the ASCII digits 0 or 1 interspersed with underscores
   */
  val BinaryIntegerLiteral:Regex = """0[bB][0-1]([0-1_]*[0-1])?[lL]?""".r
  
  /**
   * 
   * For decimal floating-point literals, at least one digit (in either the whole number or the fraction part) 
   * and either a decimal point, an exponent, or a float type suffix are required. All other parts are optional.
   * 
   * 
   *  DecimalFloatingPointLiteral = Digits [.] Digits? ExponentPart? FloatTypeSuffix?
   *							    | [.] Digits ExponentPart? FloatTypeSuffix?
   *								| Digits ExponentPart
   *								| Digits FloatTypeSuffix
   *								| Digits ExponentPart FloatTypeSuffix
   *        
   *  Where 
   *  
   *  Digits = [0-9]([0-9_]*[0-9])?
   *  ExponentPart = [eE][+\-]?[0-9]([0-9_]*[0-9])?
   *  FloatTypeSuffix = [fFdD]
   *  
   *  The composed definition looks like the below abomination.
   * 
   */
  val DecimalFloatingPointLiteral:Regex = 
    """([0-9]([0-9_]*[0-9])?[.]([0-9]([0-9_]*[0-9])?)?([eE][+\-]?[0-9]([0-9_]*[0-9])?)?[fFdD]?|[.][0-9]([0-9_]*[0-9])?([eE][+\-]?[0-9]([0-9_]*[0-9])?)?[fFdD]?|[0-9]([0-9_]*[0-9])?([eE][+\-]?[0-9]([0-9_]*[0-9])?|[fFdD]|[eE][+\-]?[0-9]([0-9_]*[0-9])?[fFdD]))""".r
    
    
  val HexadecimalFloatingPointLiteral:Regex =
    """[0][xX]([0-9a-fA-F_]([0-9a-fA-F_]*[0-9a-fA-F_])?)?[.]?[0-9a-fA-F_]([0-9a-fA-F_]*[0-9a-fA-F_])?[.]?[pP][+\-]?[0-9]([0-9_]*[0-9])?[fFdD]?""".r
  
    
  /**
   * A CharLiteral definition is composed of four parts enclosed in two 's:
   *    1. Any character rather than \n \r ' or \
   *    2. Unicode literals
   *    3. Octal litarals
   *    4. Escape sequences consisting of \b \t \n \f \r \" \' \\
   */
  val CharacterLiteral:Regex = """'([^\n\r'\\]|\\u[0-9a-fA-F]{4}+|\\([0-7][0-7]?|[0-3][0-7][0-7])|\\[btnfr"'\\])'""".r
  
  /**
   * 
   * A StringLiteral is similar to CharLiteral with two differences:
   *    1. There is a * operator allowing for repetition of characters inside "s.
   *    2. " should be escaped
   * 
   */
  val StringLiteral:Regex = """["]([^\n\r"\\]|\\u[0-9a-fA-F]{4}+|\\([0-7][0-7]?|[0-3][0-7][0-7])|\\[btnfr"'\\])*["]""".r;
  
  val BooleanLiteral:Regex = """(true|false)""".r
  
  val NullLiteral:Regex = "null".r
  
  val Comment:Regex = """(/\*(.|[\r\n])*?\*/|//[^\r\n]*)""".r
  
  val WhiteSpace:Regex = """\s""".r
  
  val Layout:Regex = """((/\*(.|[\r\n])*?\*/|//[^\r\n]*)|\s)*""".r
  
  val Keyword:Regex = """abstract|continue|for|new|switch|assert|default|if|package|synchronized|boolean|do|goto|private|this|break|double|implements|protected|throw|byte|else|import|public|throws|case|enum|instanceof|return|transient|catch|extends|int|short|try|char|final|interface|static|void|class|finally|long|strictfp|volatile|const|float|native|super|while|true|false|null""".r
  
  def javaKeywords:List[String] = 
    List("abstract", "continue", "for", "new", "switch"
		, "assert", "default", "if", "package", "synchronized"
		, "boolean", "do", "goto", "private", "this", "break"
		, "double", "implements", "protected", "throw"
		, "byte", "else", "import", "public", "throws"
		, "case", "enum", "instanceof", "return", "transient"
		, "catch", "extends", "int", "short", "try"
		, "char", "final", "interface", "static"
		, "void", "class", "finally", "long", "strictfp"
		, "volatile", "const", "float", "native", "super"
		, "while", "true", "false", "null");
  
  def main(args: Array[String]) {
    
    // identifier tests
    assert(Identifier.pattern.matcher("identifier").matches())
    assert(Identifier.pattern.matcher("Identifier").matches())
    assert(Identifier.pattern.matcher("_").matches())
    assert(Identifier.pattern.matcher("_Xyx").matches())
    assert(Identifier.pattern.matcher("x_y_z_123").matches())
    assert(Identifier.pattern.matcher("$_1_123").matches())
    assert(!Identifier.pattern.matcher("1definer").matches())
    
    // decimal number tests
    assert(DecimalIntegerLiteral.pattern.matcher("0").matches())
    assert(DecimalIntegerLiteral.pattern.matcher("1").matches())
    assert(DecimalIntegerLiteral.pattern.matcher("10").matches())
    assert(DecimalIntegerLiteral.pattern.matcher("234000").matches())
    assert(DecimalIntegerLiteral.pattern.matcher("234000L").matches())
    assert(DecimalIntegerLiteral.pattern.matcher("23_4___00_0").matches())
    assert(DecimalIntegerLiteral.pattern.matcher("23_4___00_0l").matches())
    assert(DecimalIntegerLiteral.pattern.matcher("23_4___00_0L").matches())
    assert(!DecimalIntegerLiteral.pattern.matcher("_123").matches())
    assert(!DecimalIntegerLiteral.pattern.matcher("0123").matches())
    
    // hex number tests
    assert(HexIntegerLiteral.pattern.matcher("0x0").matches())
    assert(HexIntegerLiteral.pattern.matcher("0x1a0f").matches())
    assert(HexIntegerLiteral.pattern.matcher("0x1_a0f_aL").matches())
    assert(HexIntegerLiteral.pattern.matcher("0x1_a0_f_ff_a").matches())
    assert(!HexIntegerLiteral.pattern.matcher("0x").matches())
    assert(!HexIntegerLiteral.pattern.matcher("0x_1").matches())
    
    // octal number tests
    assert(OctalIntegerLiteral.pattern.matcher("01").matches())
    assert(OctalIntegerLiteral.pattern.matcher("0_1").matches())
    assert(OctalIntegerLiteral.pattern.matcher("0___1").matches())
    assert(OctalIntegerLiteral.pattern.matcher("0_1__2").matches())
    assert(OctalIntegerLiteral.pattern.matcher("0_1__2L").matches())
    assert(OctalIntegerLiteral.pattern.matcher("0_1__2l").matches())
    assert(OctalIntegerLiteral.pattern.matcher("0123456").matches())
    assert(!OctalIntegerLiteral.pattern.matcher("0").matches())
    
    // binary number tests
    assert(BinaryIntegerLiteral.pattern.matcher("0b0").matches())
    assert(BinaryIntegerLiteral.pattern.matcher("0b011").matches())
    assert(BinaryIntegerLiteral.pattern.matcher("0b011").matches())
    assert(BinaryIntegerLiteral.pattern.matcher("0b0_11____01").matches())
    assert(BinaryIntegerLiteral.pattern.matcher("0b0_11____01L").matches())
    assert(!BinaryIntegerLiteral.pattern.matcher("0").matches())
    assert(!BinaryIntegerLiteral.pattern.matcher("0b").matches())
    assert(!BinaryIntegerLiteral.pattern.matcher("0_b").matches())
    
    // float literal tests
    assert(DecimalFloatingPointLiteral.pattern.matcher("1e1f").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("2.f").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher(".3f").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("0f").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("3.14f").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("6.022137e+23f").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("1e1").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("2.").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher(".3").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("0.0").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("3.14").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("1e-9d").matches())
    assert(DecimalFloatingPointLiteral.pattern.matcher("1e137").matches())
    
    // hexadecimal floating point tests
    assert(HexadecimalFloatingPointLiteral.pattern.matcher("0x1.8p1").matches())
    assert(HexadecimalFloatingPointLiteral.pattern.matcher("0x1.999999999999ap-4").matches())
    assert(HexadecimalFloatingPointLiteral.pattern.matcher("0x1.99____9999___9999_99ap-4").matches())
    
    // character literal tests
    assert(CharacterLiteral.pattern.matcher("'a'").matches())
    assert(CharacterLiteral.pattern.matcher("'%'").matches())
    assert(CharacterLiteral.pattern.matcher("'\t'").matches())
    assert(CharacterLiteral.pattern.matcher("'\\\\'").matches())
    assert(CharacterLiteral.pattern.matcher("'\\''").matches())
    assert(CharacterLiteral.pattern.matcher("'\\177'").matches())
    assert(CharacterLiteral.pattern.matcher("'\\u03a9'").matches())
    assert(CharacterLiteral.pattern.matcher("'\\uFFFF'").matches())
    assert(CharacterLiteral.pattern.matcher("'#'").matches())

    // String literal tests
    assert(StringLiteral.pattern.matcher("\"\"").matches())
    assert(StringLiteral.pattern.matcher("\"\\\"\"").matches())
    assert(StringLiteral.pattern.matcher("\"This is a string\"").matches())
    assert(StringLiteral.pattern.matcher("\"\\141\"").matches())
    assert(StringLiteral.pattern.matcher("\"\\57\"").matches())
    assert(StringLiteral.pattern.matcher("\"\\6\"").matches())
    
    // Comment tests
    assert(Comment.pattern.matcher("//This is a simple comment").matches())
    assert(Comment.pattern.matcher("/* aaaa */").matches())
    assert(Comment.pattern.matcher("/* this comment /* // /** ends here: */").matches())
    
    // Keyword test
    assert(Keyword.pattern.matcher("null").matches())
    assert(Keyword.pattern.matcher("abstract").matches())
    assert(Keyword.pattern.matcher("strictfp").matches())
    assert(Keyword.pattern.matcher("throws").matches())
    assert(Keyword.pattern.matcher("int").matches())
    assert(Keyword.pattern.matcher("super").matches())
    
    println("All tests pass successfully.")
  }
  
}