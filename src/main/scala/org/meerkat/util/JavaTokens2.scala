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

  val HexDigits: RegularExpression = "[0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?"
  
  val HexNumeral: RegularExpression = "0[xX]" ~ HexDigits
  
  val HexIntegerLiteral:RegularExpression = HexNumeral ~ IntegerTypeSuffix.?
  
  val OctalDigits: RegularExpression = "[0-7]([0-7_]*[0-7])?"
  
  val OctalNumeral: RegularExpression = "0[_]*" ~ OctalDigits
  
  val OctalIntegerLiteral: RegularExpression = OctalNumeral ~ IntegerTypeSuffix.?
  
  val BinaryDigits: RegularExpression = "[0-1]([0-1_]*[0-1])?"
  
  val BinaryNumeral: RegularExpression =  "0[bB]" ~  BinaryDigits
  
  val BinaryIntegerLiteral: RegularExpression = BinaryNumeral ~ IntegerTypeSuffix.?
  
  val Sign: RegularExpression = "[+-]"
  
  val SignedInteger = Sign.? ~ Digits
  
  val ExponentIndicator: RegularExpression = "[eE]";
  
  val ExponentPart: RegularExpression = ExponentIndicator ~ SignedInteger
  
  val FloatTypeSuffix: RegularExpression = "[fFdD]"

  val DecimalFloatingPointLiteral: RegularExpression = 
    ( Digits ~ "." ~ Digits.? ~ ExponentPart.? ~ FloatTypeSuffix.?
    | "." ~ Digits ~ ExponentPart.? ~ FloatTypeSuffix.?
    | Digits ~ ExponentPart
    | Digits ~ FloatTypeSuffix
    | Digits ~ ExponentPart ~ FloatTypeSuffix
    )
    
  val HexSignificand: RegularExpression = 
    ( HexNumeral
    | HexNumeral ~ "."
    | "0[xX]" ~ HexDigits.? ~ "." ~ HexDigits
    )
    
  val BinaryExponentIndicator: RegularExpression = "[pP]"  
    
  val BinaryExponent: RegularExpression = BinaryExponentIndicator ~ SignedInteger;  
    
  val HexadecimalFloatingPointLiteral: RegularExpression =  HexSignificand ~ BinaryExponent ~ FloatTypeSuffix.?  
    
  val EscapeSequence: RegularExpression = """\\[btnfr"'\\]"""
  
  val UnicodeLiteral: RegularExpression = """\\u[0-9a-fA-F]{4}+"""
  
  val OctalEscape: RegularExpression = """\\([0-7][0-7]?|[0-3][0-7][0-7])"""
  
  val SingleCharacter: RegularExpression = """[^\n\r'\\]""" | UnicodeLiteral | OctalEscape | EscapeSequence
  
  val CharacterLiteral: RegularExpression = "'" ~ SingleCharacter ~ "'"
  
  val StringCharacter: RegularExpression = """[^\n\r"\\]""" | UnicodeLiteral | OctalEscape | EscapeSequence
  
  val StringLiteral: RegularExpression = "\"" ~ StringCharacter.* ~ "\""
  
  val BooleanLiteral:RegularExpression = "true" | "false"
  
  val NullLiteral:RegularExpression = "null"
  
  val Comment:RegularExpression = """(/\*(.|[\r\n])*?\*/|//[^\r\n]*)"""
  
  val WhiteSpace:RegularExpression = """\s"""
  
  val Layout:RegularExpression = (Comment | WhiteSpace)*
  
  val Keyword:RegularExpression = javaKeywords mkString "|"
  
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