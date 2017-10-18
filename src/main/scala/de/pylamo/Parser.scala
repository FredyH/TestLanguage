package de.pylamo

import de.pylamo.language._
import de.pylamo.trees.{SData, _}
import org.apache.commons.text.StringEscapeUtils

import scala.util.Try
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}

/**
  * Created by Fredy on 12.10.2017.
  */


object Parser extends JavaTokenParsers with PackratParsers {

  //region Basic stuff

  private val keyWords: PackratParser[String] = "def" | "Int" | "Float" | "Boolean" | "if" | "else" | "data" | "Unit"

  private implicit class StringHelper(name: String) {
    //TODO: add regex for keyword boundaries
    def keyword: PackratParser[String] = ("""""".r ~> name) <~ """""".r
  }

  lazy val identifier: PackratParser[String] =
    "" ~> // handle whitespace
      (not(keyWords) ~> rep1(acceptIf(Character.isJavaIdentifierStart)("identifier expected but `" + _ + "' found"),
        elem("identifier part", Character.isJavaIdentifierPart(_: Char)))). ^^ (_.mkString)

  lazy val primitiveType: PackratParser[SType] = ("Int" | "String" | "Float" | "Unit" | "Boolean") ^^ {
    case "Float" => SFloatType
    case "Int" => SIntType
    case "String" => SStringType
    case "Unit" => SUnitType
    case "Boolean" => SBooleanType
  }

  lazy val inductiveType: PackratParser[SType] = identifier ^^ {
    name => SInductiveType(name)
  }

  lazy val types: PackratParser[SType] = primitiveType | inductiveType

  //endregion

  lazy val ofType: PackratParser[SType] = ":" ~> types

  //region Expressions

  lazy val booleanConstant: PackratParser[BooleanConstant] = ("true".keyword | "false".keyword) ^^ {
    case "true" => BooleanConstant(true)
    case "false" => BooleanConstant(false)
  }

  lazy val stringConstant: PackratParser[StringConstant] = stringLiteral ^^ {
    s => StringConstant(StringEscapeUtils.unescapeJava(s.substring(1, s.length - 1)))
  }

  lazy val numberConstant: PackratParser[SExpression] =
    (decimalNumber | floatingPointNumber) ^^ {
      case s if !s.contains(".") && Try(s.toLong).isSuccess => IntConstant(s.toLong)
      case s => FloatConstant(s.toDouble)
    }


  lazy val variable: PackratParser[SVariable] = identifier ^^ { s => SVariable(s) }
  //region Comparison operators


  private def binaryOperator[B <: BinaryOperation](token: String, func: (SExpression, SExpression) => B) =
    (expression <~ token) ~ expression ^^ { case (l ~ r) => func(l, r) }

  lazy val geq: PackratParser[GreaterEqual] = binaryOperator(">=", GreaterEqual)

  lazy val ge: PackratParser[Greater] = binaryOperator(">", Greater)

  lazy val leq: PackratParser[LessEquals] = binaryOperator("<=", LessEquals)

  lazy val le: PackratParser[Less] = binaryOperator("<", Less)

  lazy val eq: PackratParser[Equals] = binaryOperator("==", Equals)

  lazy val neq: PackratParser[NotEquals] = binaryOperator("!=", NotEquals)

  lazy val comparison: PackratParser[ComparisonOperation] = eq | neq | leq | le | geq | ge

  //endregion

  lazy val additionalArguments: PackratParser[List[SExpression]] = ("," ~> expression).*

  lazy val argumentList: PackratParser[ArgumentList] = opt(expression ~ additionalArguments) ^^ {
    case Some(e ~ l) => ArgumentList(e :: l)
    case None => ArgumentList(Nil)
  }

  lazy val functionCall: PackratParser[FunctionCallReference] = (identifier <~ "(") ~ (argumentList <~ ")") ^^ {
    case (name ~ args) => FunctionCallReference(name, args, SNoTypeYet)
  }

  lazy val cast: PackratParser[CastOperation] = (primitiveType <~ "(") ~ (expr <~ ")") ^^ {
    case (SFloatType ~ e) =>
      FloatCast(e)
    case (SIntType ~ e) =>
      IntCast(e)
    case (SStringType ~ e) =>
      StringCast(e)
  }

  lazy val factor: PackratParser[SExpression] =
    ("(" ~> expression <~ ")") | cast | functionCall | numberConstant | stringConstant | booleanConstant | variable

  lazy val term: PackratParser[SExpression] =
    factor ~ ("*" ~ factor | "/" ~ factor).* ^^ {
      case (c ~ l) => l.foldLeft(c) {
        case (left, "*" ~ right) => Multiplication(left, right)
        case (left, "/" ~ right) => Division(left, right)
      }
    }

  lazy val expr: PackratParser[SExpression] =
    term ~ ("+" ~ term | "-" ~ term).* ^^ {
      case (c ~ l) => l.foldLeft(c) {
        case (left, "+" ~ right) => Addition(left, right)
        case (left, "-" ~ right) => Subtraction(left, right)
      }
    }

  lazy val expression: PackratParser[SExpression] = comparison | expr
  //endregion

  //region Statements

  lazy val expressionStatement: PackratParser[ExpressionStatement] = expression ^^ ExpressionStatement

  lazy val ifStatement: PackratParser[IfStatement] =
    (("if".keyword ~ "(") ~> (expression <~ ")")) ~ statementList ~ opt("else".keyword ~> statementList) ^^ {
      case (cond ~ trueList ~ falseList) =>
        IfStatement(cond, trueList, falseList)
    }

  lazy val letStatement: PackratParser[SStatement] = (("let".keyword ~> identifier ~ opt(ofType)) <~ "=") ~ expression ^^ {
    case (name ~ t ~ e) => LetStatement(name, e, t)
  }

  lazy val statement: PackratParser[SStatement] = (ifStatement | letStatement | expressionStatement) <~ opt(rep(";"))
  //endregion

  //region Functions

  lazy val parameter: PackratParser[SParameter] = (identifier <~ ":") ~ types ^^ {
    case (name ~ t) => SParameter(name, t)
  }

  lazy val additionalParameters: PackratParser[List[SParameter]] = rep("," ~> parameter)

  lazy val parameterList: PackratParser[List[SParameter]] = opt(parameter ~ additionalParameters).map {
    case Some(param ~ additionals) => param :: additionals
    case None => Nil
  }

  lazy val functionName: PackratParser[String] = "def".keyword ~> identifier

  lazy val statementList: PackratParser[StatementList] = (("{" ~> statement.*) <~ "}") ^^ {
    case (l) => StatementList(l)
  }

  lazy val function: PackratParser[SFunction] =
    (functionName <~ "(") ~ (parameterList <~ ")") ~ ofType ~ statementList ^^ {
      case (name ~ params ~ funcType ~ statements) =>
        SFunction(name, funcType, ParameterList(params), statements)
    }
  //endregion

  lazy val dataCase: PackratParser[SDataCase] = identifier ~ types.* ^^ {
    case (name ~ t) => SDataCase(name, t)
  }

  lazy val data: PackratParser[SData] = (("data".keyword ~> identifier) <~ "=") ~ dataCase ~ ("|" ~> dataCase).* ^^ {
    case (name ~ c ~ l) =>
      SData(name, c :: l)
  }


  lazy val program: PackratParser[SProgram] = (function | data).* ^^ {
    l =>
      val (functions, dataDeclarations) = l.foldLeft((List.empty[SFunction], List.empty[SData])) {
        case ((funcs, datas), e: SFunction) =>
          (e :: funcs, datas)
        case ((funcs, datas), e: SData) =>
          (funcs, e :: datas)
      }
      SProgram(functions, dataDeclarations)
  }

  def parseProgram(source: String): SProgram = {
    this.parseAll(program, source) match {
      case Success(result, next) =>
        result
      case b: NoSuccess =>
        throw new RuntimeException(b.toString)
    }

  }
}
