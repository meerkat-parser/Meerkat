/*
 * Copyright (c) 2014 CWI. All rights reserved.
 * 
 * Authors:
 *     Anastasia Izmaylova  <anastasia.izmaylova@cwi.nl>
 *     Ali Afroozeh         <ali.afroozeh@cwi.nl>
 */
package java7

import meerkat._
import scala.util.matching.Regex
import java.util.HashSet
import scala.collection.JavaConversions._

object Java7_Charlevel extends MeerkatParsers {
  
    val Type: MeerkatParser = 
    "Type" ::= PrimitiveType | ReferenceType
    
    val PrimitiveType: MeerkatParser  = 
    "PrimitiveType" ::= "byte" | "short" | "char" | "int" | "long" | "float" | "double" | "boolean"
    
    val ReferenceType: MeerkatParser  = 
    "ReferenceType" ::= TypeDeclSpecifier ~ TypeArguments.? | ArrayType
    
    val ReferenceTypeNonArrayType: MeerkatParser  = 
    "ReferenceTypeNonArrayType" ::= TypeDeclSpecifier ~ TypeArguments.?
    
    val TypeList: MeerkatParser  = 
    "TypeList" ::= Type.+(',')
    
    val TypeName: MeerkatParser  = 
    "TypeName" ::= QualifiedIdentifier
    
    val TypeVariable: MeerkatParser  = 
    "TypeVariable" ::= Identifier
    
    val ArrayType: MeerkatParser  = 
    "ArrayType" ::= Type ~ '[' ~ ']'
    
    val TypeParameters: MeerkatParser  = 
    "TypeParameters" ::= '<' ~ TypeParameter.+(',') ~ '>'
    
    val TypeParameter: MeerkatParser  = 
    "TypeParameter" ::= TypeVariable ~ TypeBound.?
    
    val TypeBound: MeerkatParser  = 
    "TypeBound" ::= "extends" ~ ReferenceType.+("&")
    
    val TypeArguments: MeerkatParser  = 
    "TypeArguments" ::= '<' ~ TypeArgument.+(',') ~ '>'
    
    val TypeArgument: MeerkatParser  = 
    "TypeArgument" ::= Type | '?' ~ (("extends" | "super").gr ~ Type).gr.?
    
    val QualifiedIdentifier: MeerkatParser  = 
    "QualifiedIdentifier" ::= Identifier.+(".")
    
    val QualifiedIdentifierList: MeerkatParser  = 
    "QualifiedIdentifierList" ::= QualifiedIdentifier.+(',')
    
    val CompilationUnit: MeerkatParser  = 
    "CompilationUnit" ::= PackageDeclaration.? ~ ImportDeclaration.* ~ TypeDeclaration.*
    
    val PackageDeclaration: MeerkatParser  = 
    "PackageDeclaration" ::= Annotation.* ~ "package" ~ QualifiedIdentifier ~ ";"
    
    val ImportDeclaration: MeerkatParser  = 
    "ImportDeclaration" ::= "import" ~ "static".? ~ Identifier.+(".") ~ ('.' ~ '*').gr.? ~ ';'
    
    val TypeDeclaration: MeerkatParser  = 
    "TypeDeclaration" ::= ClassDeclaration | InterfaceDeclaration | ';'
    
    val ClassDeclaration: MeerkatParser  = 
    "ClassDeclaration" ::= NormalClassDeclaration | EnumDeclaration
    
    val NormalClassDeclaration: MeerkatParser  = 
    "NormalClassDeclaration" ::= ClassModifier.* ~ "class" ~ Identifier ~ TypeParameters.? ~ ("extends" ~ Type).gr.? ~ ("implements" ~ TypeList).gr.? ~ ClassBody
    
    val ClassModifier: MeerkatParser  = 
    "ClassModifier" ::= Annotation | "public" | "protected" | "private" | "abstract" | "static" | "final" | "strictfp"
    
    val ClassBody: MeerkatParser  = 
    "ClassBody" ::= '{' ~ ClassBodyDeclaration.* ~ '}'
    
    val ClassBodyDeclaration: MeerkatParser  = 
    "ClassBodyDeclaration" ::= ClassMemberDeclaration | InstanceInitializer | StaticInitializer | ConstructorDeclaration
    
    val InstanceInitializer: MeerkatParser  = 
    "InstanceInitializer" ::= Block
    
    val StaticInitializer: MeerkatParser  = 
    "StaticInitializer" ::= "static" ~ Block
    
    val ConstructorDeclaration: MeerkatParser  = 
    "ConstructorDeclaration" ::= ConstructorModifier.* ~ ConstructorDeclarator ~ Throws.? ~ ConstructorBody
    
    val ConstructorModifier: MeerkatParser  = 
    "ConstructorModifier" ::= Annotation | "public" | "protected" | "private"
    
    val ConstructorDeclarator: MeerkatParser  = 
    "ConstructorDeclarator" ::= TypeParameters.? ~ Identifier ~ '(' ~ FormalParameterList.? ~ ')'
    
    val ConstructorBody: MeerkatParser  = 
    "ConstructorBody" ::= '{' ~ ExplicitConstructorInvocation.? ~ BlockStatement.* ~ '}'
    
    val ExplicitConstructorInvocation: MeerkatParser  = 
    "ExplicitConstructorInvocation" ::= ( NonWildTypeArguments.? ~ "this" ~ '(' ~ ArgumentList.? ~ ')' ~ ';' 
                                        | NonWildTypeArguments.? ~ "super" ~ '(' ~ ArgumentList.? ~ ")" ~ ';' 
                                        | Primary ~ '.' ~ NonWildTypeArguments.? ~ "super" ~ '(' ~ ArgumentList.? ~ ')' ~ ';'
                                        )
    
    val NonWildTypeArguments: MeerkatParser  = 
    "NonWildTypeArguments" ::= '<' ~ ReferenceType.+(',') ~ '>'
    
    val ClassMemberDeclaration: MeerkatParser  = 
    "ClassMemberDeclaration" ::= FieldDeclaration | MethodDeclaration | ClassDeclaration | InterfaceDeclaration | ';'
    
    val InterfaceDeclaration: MeerkatParser  = 
    "InterfaceDeclaration" ::= NormalInterfaceDeclaration | AnnotationTypeDeclaration
    
    val NormalInterfaceDeclaration: MeerkatParser  = 
    "NormalInterfaceDeclaration" ::= InterfaceModifier.* ~ "interface" ~ Identifier ~ TypeParameters.? ~ ("extends" ~ TypeList).gr.? ~ InterfaceBody
    
    val InterfaceModifier: MeerkatParser  = 
    "InterfaceModifier" ::= Annotation | "public" | "protected" | "private" | "abstract" | "static" | "strictfp"
    
    val InterfaceBody: MeerkatParser  = 
    "InterfaceBody" ::= "{" ~ InterfaceMemberDeclaration.* ~ "}"
    
    val InterfaceMemberDeclaration: MeerkatParser  = 
    "InterfaceMemberDeclaration" ::= ConstantDeclaration | AbstractMethodDeclaration | ClassDeclaration | InterfaceDeclaration | ';'
    
    val ConstantDeclaration: MeerkatParser  = 
    "ConstantDeclaration" ::= ConstantModifier.* ~ Type ~ VariableDeclarators ~ ';'
    
    val ConstantModifier: MeerkatParser  = 
    "ConstantModifier" ::= Annotation | "public" | "static" | "final"
    
    val AbstractMethodDeclaration: MeerkatParser  = 
    "AbstractMethodDeclaration" ::= AbstractMethodModifier.* ~ TypeParameters.? ~ Result ~ MethodDeclarator ~ Throws.? ~ ';'
    
    val AbstractMethodModifier: MeerkatParser  = 
    "AbstractMethodModifier" ::= Annotation | "public" | "abstract"
    
    val AnnotationTypeDeclaration: MeerkatParser  = 
    "AnnotationTypeDeclaration" ::= InterfaceModifier.* ~ '@' ~ "interface" ~ Identifier ~ AnnotationTypeBody
    
    val AnnotationTypeBody: MeerkatParser  = 
    "AnnotationTypeBody" ::= '{' ~ AnnotationTypeElementDeclaration.* ~ '}'
    
    val AnnotationTypeElementDeclaration: MeerkatParser  = 
    "AnnotationTypeElementDeclaration" ::= ( AbstractMethodModifier.* ~ Type ~ Identifier ~ '(' ~ ')' ~ ('[' ~ ']').gr.* ~ DefaultValue.? ~ ';' 
                                           | ConstantDeclaration | ClassDeclaration | InterfaceDeclaration | AnnotationTypeDeclaration | ';'
                                           )
    
    val DefaultValue: MeerkatParser  = 
    "DefaultValue" ::= "default" ~ ElementValue
    
    val FieldDeclaration: MeerkatParser  = 
    "FieldDeclaration" ::= FieldModifier.* ~ Type ~ VariableDeclarators ~ ';'
    
    val FieldModifier: MeerkatParser  = 
    "FieldModifier" ::= Annotation | "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile"
    
    val VariableDeclarators: MeerkatParser  = 
    "VariableDeclarators" ::= VariableDeclarator.+(',')
    
    val VariableDeclarator: MeerkatParser  = 
    "VariableDeclarator" ::= VariableDeclaratorId ~ ('=' ~ VariableInitializer).gr.?
    
    val VariableDeclaratorId: MeerkatParser  = 
    "VariableDeclaratorId" ::= Identifier ~ ('[' ~ ']').gr.*
    
    val VariableInitializer: MeerkatParser  = 
    "VariableInitializer" ::= ArrayInitializer | Expression
    
    val ArrayInitializer: MeerkatParser  = 
    "ArrayInitializer" ::= '{' ~ VariableInitializer.*(terminal(',')) ~ ','.? ~ '}'
    
    val MethodDeclaration: MeerkatParser  = 
    "MethodDeclaration" ::= MethodHeader ~ MethodBody
    
    val MethodHeader: MeerkatParser  = 
    "MethodHeader" ::= MethodModifier.* ~ TypeParameters.? ~ Result ~ MethodDeclarator ~ Throws.?
    
    val MethodDeclarator: MeerkatParser  = 
    "MethodDeclarator" ::= Identifier ~ '(' ~ FormalParameterList.? ~ ')' | MethodDeclarator ~ '[' ~ ']'
    
    val FormalParameterList: MeerkatParser  = 
    "FormalParameterList" ::= (FormalParameter ~ ',').gr.* ~ LastFormalParameter
    
    val FormalParameter: MeerkatParser  = 
    "FormalParameter" ::= VariableModifier.* ~ Type ~ VariableDeclaratorId
    
    val VariableModifier: MeerkatParser  = 
    "VariableModifier" ::= "final" | Annotation
    
    val LastFormalParameter: MeerkatParser  = 
    "LastFormalParameter" ::= VariableModifier.* ~ Type ~ "..." ~ VariableDeclaratorId | FormalParameter
    
    val MethodModifier: MeerkatParser  = 
    "MethodModifier" ::= Annotation | "public" | "protected" | "private" | "abstract" | "static" | "final" | "synchronized" | "native" | "strictfp"
    
    val Result: MeerkatParser  = 
    "Result" ::= Type | "void"
    
    val Throws: MeerkatParser  = 
    "Throws" ::= "throws" ~ ExceptionType.+(',')
    
    val ExceptionType: MeerkatParser  = 
    "ExceptionType" ::= TypeName
    
    val MethodBody: MeerkatParser  = 
    "MethodBody" ::= Block | ';'
    
    val Annotation: MeerkatParser  = 
    "Annotation" ::= ( '@' ~ TypeName ~ '(' ~ ElementValuePair.*(terminal(',')) ~ ')' 
                     | '@' ~ TypeName ~ ('(' ~ ElementValue ~ ')').gr.?
                     )
    
    val ElementValuePair: MeerkatParser  = 
    "ElementValuePair" ::= Identifier ~ '=' ~ ElementValue
    
    val ElementValue: MeerkatParser  = 
    "ElementValue" ::= ConditionalExpression | Annotation | ElementValueArrayInitializer
    
    val ElementValueArrayInitializer: MeerkatParser  = 
    "ElementValueArrayInitializer" ::= '{' ~ ElementValues.? ~ ','.? ~ '}'
    
    val ElementValues: MeerkatParser  = 
    "ElementValues" ::= ElementValue.+(',')
    
    val EnumDeclaration: MeerkatParser  = 
    "EnumDeclaration" ::= ClassModifier.* ~ "enum" ~ Identifier ~ ("implements" ~ TypeList).gr.? ~ EnumBody
    
    val EnumBody: MeerkatParser  = 
    "EnumBody" ::= "{" ~ EnumConstant.*(terminal(',')) ~ ",".? ~ EnumBodyDeclarations.? ~ "}"
    
    val EnumConstant: MeerkatParser  = 
    "EnumConstant" ::= Annotation.* ~ Identifier ~ Arguments.? ~ ClassBody.?
    
    val Arguments: MeerkatParser  = 
    "Arguments" ::= "(" ~ ArgumentList.? ~ ")"
    
    val EnumBodyDeclarations: MeerkatParser  = 
    "EnumBodyDeclarations" ::= ";" ~ ClassBodyDeclaration.*
    
    val Block: MeerkatParser  = 
    "Block" ::= "{" ~ BlockStatement.* ~ "}"
    
    val BlockStatement: MeerkatParser  = 
    "BlockStatement" ::= LocalVariableDeclarationStatement | ClassDeclaration | Statement
    
    val LocalVariableDeclarationStatement: MeerkatParser  = 
    "LocalVariableDeclarationStatement" ::= VariableModifier.* ~ Type ~ VariableDeclarators ~ ";"
    
    val Statement: MeerkatParser  = 
    "Statement" ::= ( StatementWithoutTrailingSubstatement 
                    | Identifier ~ ":" ~ Statement 
                    | "if" ~ "(" ~ Expression ~ ")" ~ Statement 
                    | "if" ~ "(" ~ Expression ~ ")" ~ StatementNoShortIf ~ "else" ~ Statement 
                    | "while" ~ "(" ~ Expression ~ ")" ~ Statement 
                    | ForStatement
                    )
    
    val StatementWithoutTrailingSubstatement: MeerkatParser  = 
    "StatementWithoutTrailingSubstatement" ::= ( Block 
                                               | ";" 
                                               | StatementExpression ~ ";" 
                                               | "assert" ~ Expression ~ (":" ~ Expression).gr.? ~ ";" 
                                               | "switch" ~ "(" ~ Expression ~ ")" ~ "{" ~ SwitchBlockStatementGroup.* ~ SwitchLabel.* ~ "}" 
                                               | "do" ~ Statement ~ "while" ~ "(" ~ Expression ~ ")" ~ ";" 
                                               | "break" ~ Identifier.? ~ ";" 
                                               | "continue" ~ Identifier.? ~ ";" 
                                               | "return" ~ Expression.? ~ ";" 
                                               | "synchronized" ~ "(" ~ Expression ~ ")" ~ Block 
                                               | "throw" ~ Expression ~ ";" 
                                               | "try" ~ Block ~ (CatchClause.+ | (CatchClause.* ~ Finally).gr).gr 
                                               | "try" ~ ResourceSpecification ~ Block ~ CatchClause.* ~ Finally.?
                                               )
    
    val StatementNoShortIf: MeerkatParser  = 
    "StatementNoShortIf" ::= ( StatementWithoutTrailingSubstatement 
                             | Identifier ~ ":" ~ StatementNoShortIf 
                             | "if" ~ "(" ~ Expression ~ ")" ~ StatementNoShortIf ~ "else" ~ StatementNoShortIf 
                             | "while" ~ "(" ~ Expression ~ ")" ~ StatementNoShortIf 
                             | "for" ~ "(" ~ ForInit.? ~ ";" ~ Expression.? ~ ";" ~ ForUpdate.? ~ ")" ~ StatementNoShortIf
                             )
    
    val ForStatement: MeerkatParser  = 
    "ForStatement" ::= ( "for" ~ "(" ~ ForInit.? ~ ";" ~ Expression.? ~ ";" ~ ForUpdate.? ~ ")" ~ Statement 
                       | "for" ~ "(" ~ FormalParameter ~ ":" ~ Expression ~ ")" ~ Statement
                       )
    
    val StatementExpression: MeerkatParser  = 
    "StatementExpression" ::= Assignment | PreIncrementExpression | PreDecrementExpression | PostIncrementExpression | PostDecrementExpression | MethodInvocation | ClassInstanceCreationExpression
    
    val CatchClause: MeerkatParser  = 
    "CatchClause" ::= "catch" ~ "(" ~ VariableModifier.* ~ CatchType ~ Identifier ~ ")" ~ Block
    
    val CatchType: MeerkatParser  = 
    "CatchType" ::= QualifiedIdentifier.+("|")
    
    val Finally: MeerkatParser  = 
    "Finally" ::= "finally" ~ Block
    
    val ResourceSpecification: MeerkatParser  = 
    "ResourceSpecification" ::= "(" ~ Resources ~ ";".? ~ ")"
    
    val Resources: MeerkatParser  = 
    "Resources" ::= Resource.+(";")
    
    val Resource: MeerkatParser  = 
    "Resource" ::= VariableModifier.* ~ ReferenceType ~ VariableDeclaratorId ~ "=" ~ Expression
    
    val SwitchBlockStatementGroup: MeerkatParser  = 
    "SwitchBlockStatementGroup" ::= SwitchLabel.+ ~ BlockStatement.+
    
    val SwitchLabel: MeerkatParser  = 
    "SwitchLabel" ::= "case" ~ ConstantExpression ~ ":" | "default" ~ ":"
    
    val LocalVariableDeclaration: MeerkatParser  = 
    "LocalVariableDeclaration" ::= VariableModifier.* ~ Type ~ VariableDeclarator.+(',')
    
    val ForInit: MeerkatParser  = 
    "ForInit" ::= StatementExpression.+(',') | LocalVariableDeclaration
    
    val ForUpdate: MeerkatParser  = 
    "ForUpdate" ::= StatementExpression.+(',')
    
    val Primary: MeerkatParser  = 
    "Primary" ::= PrimaryNoNewArray | ArrayCreationExpression
    
    val PrimaryNoNewArray: MeerkatParser  = 
    "PrimaryNoNewArray" ::= ( Literal 
    						| Type ~ "." ~ "class" 
    						| "void" ~ "." ~ "class" 
    						| "this" 
    						| ClassName ~ "." ~ "this" 
    						| "(" ~ Expression ~ ")" 
    						| ClassInstanceCreationExpression 
    						| FieldAccess 
    						| MethodInvocation 
    						| ArrayAccess
    						)
    
    val Literal: MeerkatParser  = 
    "Literal" ::= IntegerLiteral | FloatingPointLiteral | BooleanLiteral | CharacterLiteral | StringLiteral | NullLiteral
    
    val IntegerLiteral: MeerkatParser  = 
    "IntegerLiteral" ::= ( DecimalIntegerLiteral.!>>('.') 
                         | HexIntegerLiteral.!>>('.') 
                         | OctalIntegerLiteral 
                         | BinaryIntegerLiteral
                         )
    
    val FloatingPointLiteral: MeerkatParser  = 
    "FloatingPointLiteral" ::= DecimalFloatingPointLiteral | HexadecimalFloatingPointLiteral
    
    val ClassInstanceCreationExpression: MeerkatParser  = 
    "ClassInstanceCreationExpression" ::= ( "new" ~ TypeArguments.? ~ TypeDeclSpecifier ~ TypeArgumentsOrDiamond.? ~ "(" ~ ArgumentList.? ~ ")" ~ ClassBody.? 
                                          | (Primary | QualifiedIdentifier).gr ~ "." ~ "new" ~ TypeArguments.? ~ Identifier ~ TypeArgumentsOrDiamond.? ~ "(" ~ ArgumentList.? ~ ")" ~ ClassBody.?
                                          )
    
    val TypeArgumentsOrDiamond: MeerkatParser  = 
    "TypeArgumentsOrDiamond" ::= "<" ~ ">" | TypeArguments
    
    val ArgumentList: MeerkatParser  = 
    "ArgumentList" ::= Expression.+(',')
    
    val ArrayCreationExpression: MeerkatParser  = 
    "ArrayCreationExpression" ::= ( "new" ~ (PrimitiveType | ReferenceType).gr ~ DimExpr.+ ~ ("[" ~ "]").gr.* 
                                  | "new" ~ (PrimitiveType | ReferenceTypeNonArrayType).gr ~ ("[" ~ "]").gr.+ ~ ArrayInitializer
                                  )
    
    val DimExpr: MeerkatParser  = 
    "DimExpr" ::= "[" ~ Expression ~ "]"
    
    val FieldAccess: MeerkatParser  = 
    "FieldAccess" ::= ( Primary ~ "." ~ Identifier 
                      | "super" ~ "." ~ Identifier 
                      | ClassName ~ "." ~ "super" ~ "." ~ Identifier
                      )
    
    val MethodInvocation: MeerkatParser  = 
    "MethodInvocation" ::= ( MethodName ~ "(" ~ ArgumentList.? ~ ")" 
                           |   Primary ~ "." ~ NonWildTypeArguments.? ~ Identifier ~ "(" ~ ArgumentList.? ~ ")" 
                           | "super" ~ "." ~ NonWildTypeArguments.? ~ Identifier ~ "(" ~ ArgumentList.? ~ ")" 
                           | ClassName ~ "." ~ "super" ~ "." ~ NonWildTypeArguments.? ~ Identifier ~ "(" ~ ArgumentList.? ~ ")" 
                           | TypeName ~ "." ~ NonWildTypeArguments ~ Identifier ~ "(" ~ ArgumentList.? ~ ")"
                           )
    
    val ArrayAccess: MeerkatParser  = 
    "ArrayAccess" ::= ExpressionName ~ "[" ~ Expression ~ "]" | PrimaryNoNewArray ~ "[" ~ Expression ~ "]"
    
    val PostfixExpression: MeerkatParser  = 
    "PostfixExpression" ::= Primary | ExpressionName | PostIncrementExpression | PostDecrementExpression
    
    val PostIncrementExpression: MeerkatParser  = 
    "PostIncrementExpression" ::= PostfixExpression ~ "++"
    
    val PostDecrementExpression: MeerkatParser  = 
    "PostDecrementExpression" ::= PostfixExpression ~ "--"
    
    val UnaryExpression: MeerkatParser  = 
    "UnaryExpression" ::= ( PreIncrementExpression 
                          | PreDecrementExpression 
                          | '+'.!>>('+') ~ UnaryExpression 
                          | '-'.!>>('-') ~ UnaryExpression 
                          | UnaryExpressionNotPlusMinus
                          )
    
    val PreIncrementExpression: MeerkatParser  = 
    "PreIncrementExpression" ::= "++" ~ UnaryExpression
    
    val PreDecrementExpression: MeerkatParser  = 
    "PreDecrementExpression" ::= "--" ~ UnaryExpression
    
    val UnaryExpressionNotPlusMinus: MeerkatParser  = 
    "UnaryExpressionNotPlusMinus" ::= PostfixExpression | '~' ~ UnaryExpression | '!' ~ UnaryExpression | CastExpression
    
    val CastExpression: MeerkatParser  = 
    "CastExpression" ::= '(' ~ PrimitiveType ~ ')' ~ UnaryExpression | '(' ~ ReferenceType ~ ')' ~ UnaryExpressionNotPlusMinus
    
    val MultiplicativeExpression: MeerkatParser  = 
    "MultiplicativeExpression" ::= ( UnaryExpression 
    							   | MultiplicativeExpression ~ '*' ~ UnaryExpression 
    							   | MultiplicativeExpression ~ '/' ~ UnaryExpression 
    							   | MultiplicativeExpression ~ '%' ~ UnaryExpression
    							   )
    
    val AdditiveExpression: MeerkatParser  = 
    "AdditiveExpression" ::= ( MultiplicativeExpression 
    						 | AdditiveExpression ~ '+'.!>>('+') ~ MultiplicativeExpression 
    						 | AdditiveExpression ~ '-'.!>>('-') ~ MultiplicativeExpression
    						 )
    
    val ShiftExpression: MeerkatParser  = 
    "ShiftExpression" ::= ( AdditiveExpression 
    					  | ShiftExpression ~ "<<" ~ AdditiveExpression 
    					  | ShiftExpression ~ ">>" ~ AdditiveExpression 
    					  | ShiftExpression ~ ">>>" ~ AdditiveExpression
    					  )
    
    val RelationalExpression: MeerkatParser  = 
    "RelationalExpression" ::= ( ShiftExpression 
    						   | RelationalExpression ~ "<" ~ ShiftExpression 
    						   | RelationalExpression ~ ">" ~ ShiftExpression 
    						   | RelationalExpression ~ "<=" ~ ShiftExpression 
    						   | RelationalExpression ~ ">=" ~ ShiftExpression 
    						   | RelationalExpression ~ "instanceof" ~ ReferenceType
    						   )
    
    val EqualityExpression: MeerkatParser  = 
    "EqualityExpression" ::= ( RelationalExpression 
    						 | EqualityExpression ~ "==" ~ RelationalExpression 
    						 | EqualityExpression ~ "!=" ~ RelationalExpression
    						 )
    
    val AndExpression: MeerkatParser  = 
    "AndExpression" ::= ( EqualityExpression 
    					| AndExpression ~ '&' ~ EqualityExpression
    					)
    
    val ExclusiveOrExpression: MeerkatParser  = 
    "ExclusiveOrExpression" ::= ( AndExpression 
    							| ExclusiveOrExpression ~ '^' ~ AndExpression
    							)
    
    val InclusiveOrExpression: MeerkatParser  = 
    "InclusiveOrExpression" ::= ( ExclusiveOrExpression 
    							| InclusiveOrExpression ~ '|' ~ ExclusiveOrExpression
    							)
    
    val ConditionalAndExpression: MeerkatParser  = 
    "ConditionalAndExpression" ::= ( InclusiveOrExpression 
    							   | ConditionalAndExpression ~ "&&" ~ InclusiveOrExpression
    							   )
    
    val ConditionalOrExpression: MeerkatParser  = 
    "ConditionalOrExpression" ::= ( ConditionalAndExpression 
    							  | ConditionalOrExpression ~ "||" ~ ConditionalAndExpression
    							  )
    
    val ConditionalExpression: MeerkatParser  = 
    "ConditionalExpression" ::= ( ConditionalOrExpression 
    							| ConditionalOrExpression ~ '?' ~ Expression ~ ':' ~ ConditionalExpression
    							)
    
    val AssignmentExpression: MeerkatParser  = 
    "AssignmentExpression" ::= ( ConditionalExpression 
    						   | Assignment
    						   )
    
    val Assignment: MeerkatParser  = 
    "Assignment" ::= LeftHandSide ~ AssignmentOperator ~ AssignmentExpression
    
    val LeftHandSide: MeerkatParser  = 
    "LeftHandSide" ::= ( ExpressionName 
    				   | '(' ~ LeftHandSide ~ ')' 
    				   | FieldAccess 
    				   | ArrayAccess
    				   )
    
    val AssignmentOperator: MeerkatParser  = 
    "AssignmentOperator" ::= "=" | "+=" | "-=" | "*=" | "/=" | "&=" | "|=" | "^=" | "%=" | "<<=" | ">>=" | ">>>="
    
    val Expression: MeerkatParser  = 
    "Expression" ::= AssignmentExpression
    
    val ConstantExpression: MeerkatParser  = 
    "ConstantExpression" ::= Expression
    
    val ClassName: MeerkatParser  = 
    "ClassName" ::= QualifiedIdentifier
    
    val ExpressionName: MeerkatParser  = 
    "ExpressionName" ::= QualifiedIdentifier
    
    val MethodName: MeerkatParser  = 
    "MethodName" ::= QualifiedIdentifier
    
    val TypeDeclSpecifier: MeerkatParser  = 
    "TypeDeclSpecifier" ::= Identifier ~ (TypeArguments.? ~ '.' ~ Identifier).gr.*
    
    val SuperSuffix: MeerkatParser  = 
    "SuperSuffix" ::= Arguments | '.' ~ Identifier ~ Arguments.?
    
    val ExplicitGenericInvocationSuffix: MeerkatParser  = 
    "ExplicitGenericInvocationSuffix" ::= "super" ~ SuperSuffix | Identifier ~ Arguments
    
  
    // Lexical definitions
    
    val UnicodeInputCharacter: MeerkatParser = 
    "UnicodeInputCharacter" ::= UnicodeEscape | RawInputCharacter
    
    val u: MeerkatParser = 'u'
    
    val UnicodeEscape: MeerkatParser = 
    "UnicodeEscape" ::= '\\' ~~ u.++ ~~ HexDigit ~~ HexDigit ~~ HexDigit ~~ HexDigit
    
    val RawInputCharacter: MeerkatParser = 
    "RawInputCharacter" ::= ('\\'-!'\\') | "\\".!>>("""[u\\]""".r) | "\\\\"
    
    val InputCharacter: MeerkatParser = 
    "InputCharacter" ::= UnicodeInputCharacter.\('\r', '\n') | '\u0000'
    
    val WhiteSpace: MeerkatParser = 
    "WhiteSpace" ::= charClass(' ', '\t', '\n' , '\r', '\f', '\u0a1a')()
    
    val LineTerminator: MeerkatParser = 
    "LineTerminator" ::= charClass('\r', '\n')()
    
    val Comment: MeerkatParser = 
    "Comment" ::= TraditionalComment | EndOfLineComment
    
    val TraditionalComment: MeerkatParser = 
    "TraditionalComment" ::= "/*" ~~ CommentTail
    
    val EndOfLineComment: MeerkatParser = 
    "EndOfLineComment" ::= "//" ~~ InputCharacter.**.!>>(""".""".r)
    
    val CommentTail: MeerkatParser = 
    "CommentTail" ::= '*' ~~ CommentTailStar | NotStar ~~ CommentTail
    
//  val CommentTail: MeerkatParser = 
//  "CommentTail" ::= NotStar.** ~~ '*' ~~ CommentTailStar

    val CommentTailStar: MeerkatParser = 
    "CommentTailStar" ::= '/' | '*' ~~ CommentTailStar | NotStarNotSlash ~~ CommentTail
    
    val NotStar: MeerkatParser = 
    "NotStar" ::= InputCharacter.\('*') | LineTerminator
    
    val NotStarNotSlash: MeerkatParser = 
    "NotStarNotSlash" ::= InputCharacter.\('*', '/') | LineTerminator
    
    val Identifier: MeerkatParser = 
    "Identifier" ::= IdentifierChars.\(Keyword).!>>("""[$0-9A-Z_a-z]""".r).!<<("""[$0-9A-Z_a-z]""".r)
    
    val IdentifierChars: MeerkatParser = 
    "IdentifierChars" ::= JavaLetter | IdentifierChars ~~ JavaLetterOrDigit
    
    val JavaLetter: MeerkatParser = 
    "JavaLetter" ::= charClass('$', '_')(('a', 'z'), ('A', 'Z'))
    
    val JavaLetterOrDigit: MeerkatParser = 
    "JavaLetterOrDigit" ::= charClass('$', '_')(('a', 'z'), ('A', 'Z'), ('0', '9'))
    
    
    val DecimalIntegerLiteral: MeerkatParser = 
    "DecimalIntegerLiteral" ::= DecimalNumeral ~~ IntegerTypeSuffix.?
    
    val HexIntegerLiteral: MeerkatParser = 
    "OctalIntegerLiteral" ::= HexNumeral ~~ IntegerTypeSuffix.?
    
    val OctalIntegerLiteral: MeerkatParser = 
    "OctalIntegerLiteral" ::= OctalNumeral ~~ IntegerTypeSuffix.?
    
    val BinaryIntegerLiteral: MeerkatParser = 
    "BinaryIntegerLiteral" ::= BinaryNumeral ~~ IntegerTypeSuffix.?
    
    val IntegerTypeSuffix: MeerkatParser = 
    "IntegerTypeSuffix" ::= charClass('l', 'L')()
    
    val underscore: MeerkatParser = '_'
    
    val DecimalNumeral: MeerkatParser = 
    "DecimalNumeral" ::= '0' | NonZeroDigit ~~ Digits.? | NonZeroDigit ~~ underscore.++ ~~ Digits
    
    val Digits: MeerkatParser = 
    "Digits" ::= Digit | Digit ~~ DigitOrUnderscore.** ~~ Digit
    
    val Digit: MeerkatParser = 
    "Digit" ::= '0' | NonZeroDigit
    
    val NonZeroDigit: MeerkatParser = 
    "NonZeroDigit" ::= '1'--'9'
    
    val DigitOrUnderscore: MeerkatParser = 
    "DigitOrUnderscore" ::= Digit | '_'
    
    val HexNumeral: MeerkatParser = 
    "HexNumeral" ::= '0' ~~ 'x' ~~ HexDigits | '0' ~~ 'X' ~~ HexDigits
    
    val HexDigits: MeerkatParser = 
    "HexDigits" ::= HexDigit | HexDigit ~~ HexDigitOrUnderscore.** ~~ HexDigit
    
    val HexDigit: MeerkatParser = 
    "HexDigit" ::= charClass()(('0','9'), ('a','f'), ('A','F'))
    
    val HexDigitOrUnderscore: MeerkatParser = 
    "HexDigitOrUnderscore" ::= HexDigit | '_'
    
    val OctalNumeral: MeerkatParser = 
    "OctalNumeral" ::= '0' ~~ OctalDigits | '0' ~~ underscore.++ ~~ OctalDigits
    
    val OctalDigits: MeerkatParser = 
    "OctalDigits" ::= OctalDigit | OctalDigit ~~ OctalDigitOrUnderscore.** ~~ OctalDigit
    
    val OctalDigit: MeerkatParser = 
    "OctalDigit" ::= charClass()(('0','7'))
    
    val OctalDigitOrUnderscore: MeerkatParser = 
    "OctalDigitOrUnderscore" ::= OctalDigit | '_'
    
    val BinaryNumeral: MeerkatParser = 
    "BinaryNumeral" ::= '0' ~~ 'b' ~~ BinaryDigits | '0' ~~ 'B' ~~ BinaryDigits
    
    val BinaryDigits: MeerkatParser = 
    "BinaryDigits" ::= BinaryDigit | BinaryDigit ~~ BinaryDigitOrUnderscore.** ~~ BinaryDigit
    
    val BinaryDigit: MeerkatParser = 
    "BinaryDigit" ::= charClass('0','1')()
    
    val BinaryDigitOrUnderscore: MeerkatParser = 
    "BinaryDigitOrUnderscore" ::= BinaryDigit | '_'
    
    val DecimalFloatingPointLiteral: MeerkatParser = 
    "DecimalFloatingPointLiteral" ::= ( Digits ~~ '.' ~~ Digits.? ~~ ExponentPart.? ~~ FloatTypeSuffix.? 
                                      | '.' ~~ Digits ~~ ExponentPart.? ~~ FloatTypeSuffix.? 
                                      | Digits ~~ ExponentPart 
                                      | Digits ~~ FloatTypeSuffix 
                                      | Digits ~~ ExponentPart ~~ FloatTypeSuffix
                                      )
    
    val ExponentPart: MeerkatParser = 
    "ExponentPart" ::= ExponentIndicator ~~ SignedInteger
    
    val ExponentIndicator: MeerkatParser = 
    "ExponentIndicator" ::= charClass('e', 'E')()
    
    val SignedInteger: MeerkatParser = 
    "SignedInteger" ::= Sign.? ~~ Digits
    
    val Sign: MeerkatParser = 
    "Sign" ::= charClass('+', '-')()
    
    val FloatTypeSuffix: MeerkatParser = 
    "FloatTypeSuffix" ::= charClass('f', 'F', 'd', 'D')()
    
    val HexadecimalFloatingPointLiteral: MeerkatParser = 
    "FloatTypeSuffix" ::= HexSignificand ~~ BinaryExponent ~~ FloatTypeSuffix.?
    
    val HexSignificand: MeerkatParser = 
    "HexSignificand" ::= ( HexNumeral 
                         | HexNumeral ~~ '.' 
                         | '0' ~~ 'x' ~~ HexDigits.? ~~ '.' ~~ HexDigits 
                         | '0' ~~ 'X' ~~ HexDigits.? ~~ '.' ~~ HexDigits
                         )
    
    val BinaryExponent: MeerkatParser = 
    "BinaryExponent" ::= BinaryExponentIndicator ~~ SignedInteger
    
    val BinaryExponentIndicator: MeerkatParser = 
    "BinaryExponentIndicator" ::= charClass('p', 'P')()
    
    val BooleanLiteral: MeerkatParser = 
    "BooleanLiteral" ::= "true" | "false"
    
    val CharacterLiteral: MeerkatParser = 
    "BooleanLiteral" ::= '\'' ~~ SingleCharacter ~~ '\'' | '\'' ~~ EscapeSequence ~~ '\''
    
    val SingleCharacter: MeerkatParser = 
    "SingleCharacter" ::= InputCharacter.\(''', '\\')
    
    val StringLiteral: MeerkatParser = 
    "StringLiteral" ::= '"' ~~ StringCharacter.** ~~ '"'
    
    val StringCharacter: MeerkatParser = 
    "StringCharacter" ::= InputCharacter.\('"', '\\') | EscapeSequence
    
    val EscapeSequence: MeerkatParser = 
    "EscapeSequence" ::= ( Backslash ~~ 'b' 
                         | Backslash ~~ 't' 
                         | Backslash ~~ 'n' 
                         | Backslash ~~ 'f' 
                         | Backslash ~~ 'r' 
                         | Backslash ~~ '"' 
                         | Backslash ~~ '\'' 
                         | '\\' ~~ u.++ ~~ "005" ~~ charClass('c', 'C')() ~~ '\\' ~~ u.++ ~~ "005" ~~ charClass('c', 'C')() 
                         | OctalEscape
                         )
    
    val Backslash: MeerkatParser = 
    "Backslash" ::= '\\' ~~ u.++ ~~ "005" ~~ charClass('c', 'C')() | '\\'
    
    
    val OctalEscape: MeerkatParser = 
    "OctalEscape" ::= ( '\\' ~~ OctalDigit.!>>("""[0-7]""".r) 
                      | '\\' ~~ OctalDigit ~~ OctalDigit.!>>("""[0-7]""".r) 
                      | '\\' ~~ ZeroToThree ~~ OctalDigit ~~ OctalDigit
                      )
    
    val ZeroToThree: MeerkatParser = 
    "ZeroToThree" ::= charClass('0', '1', '2', '3')()
    
    val NullLiteral: MeerkatParser = 
    "NullLiteral" ::= "null"
    
    
//    val Keyword:Regex = """abstract|continue|for|new|switch|assert|default|if|package|synchronized|boolean|do|goto|private|this|break|double|implements|protected|throw|byte|else|import|public|throws|case|enum|instanceof|return|transient|catch|extends|int|short|try|char|final|interface|static|void|class|finally|long|strictfp|volatile|const|float|native|super|while|true|false|null""".r
      
    val Keyword = {
      val s: scala.collection.mutable.Set[String] = new java.util.HashSet[String]()
      val l = List( "abstract", "continue", "for", "new", "switch", "assert", "default", "if", "package", "synchronized", "boolean", "do", "goto", "private", "this", "break", "double", "implements", "protected", "throw", "byte", "else", "import", "public", "throws", "case", "enum", "instanceof", "return", "transient", "catch", "extends", "int", "short", "try", "char", "final", "interface", "static", "void", "class", "finally", "long", "strictfp", "volatile", "const", "float", "native", "super", "while", "true", "false", "null")
      for (k <- l) {
        s += k
      }
      s
    }
    
    // (WhiteSpace | Comment)* !>> [\t \n \r \f  \ ] !>> "/*" !>> "//";
    override val Layout: MeerkatParser = 
    "L" ::= ((Comment | WhiteSpace).gr.**).!>>("\t", "\n", "\r", "\f", " ", "/*", "//")
    
    
  import util.Configuration._
  
  def main(args: Array[String]) {
     val input = scala.io.Source.fromFile("test-files/test.java").mkString              
     parse(input, start(CompilationUnit), ALL_PARSES, TESTING)
  }
  
}
