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
import org.meerkat.util.RegularExpression
import org.meerkat.util.RegularExpression._
import org.meerkat.util.Char
import org.meerkat.util.Char._

object JavaTokens2 {

  val JavaLetter:RegularExpression = 'a'--'z' | 'A'--'Z' | '_' | '$'
  
  val Digit: RegularExpression = '0'--'9'

  val Identifier:RegularExpression = JavaLetter ~ (JavaLetter | Digit).*

  val IntegerTypeSuffix: RegularExpression = "l|L"

  val Digits: RegularExpression = "[0-9]([0-9_]*[0-9])?"
  
  val NonZeroDigit: RegularExpression = '1'--'9'
  
  val DecimalNumeral: RegularExpression =  "0" | NonZeroDigit ~ Digits.? | NonZeroDigit ~ "_".+() ~ Digits
  
  val DecimalIntegerLiteral = DecimalNumeral ~ IntegerTypeSuffix.?

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
 
}