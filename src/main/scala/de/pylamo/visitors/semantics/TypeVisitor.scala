package de.pylamo.visitors.semantics

import de.pylamo.language._
import de.pylamo.trees._
import de.pylamo.visitors.TreeBuildVisitor

/**
  * Created by Fredy on 12.10.2017.
  */
//TODO: expectedType not used yet, might prove useful later
case class TypeVisitorInfo(program: SProgram, expectedType: SType, var variableTypes: Map[String, SType])

object TypeVisitor extends TreeBuildVisitor[TypeVisitorInfo] {

  private def findDataOrCase(name: String, program: SProgram): Either[SData, SDataCase] = {
    val dataDeclaration = program.dataDeclarations.find(_.name == name)
    val dataCase = program.dataDeclarations.flatMap(_.cases).find(_.name == name)
    (dataDeclaration, dataCase) match {
      case (_, Some(c)) => Right(c)
      case (Some(c), _) => Left(c)
    }
  }

  private def findCommonSuperType(left: SType, right: SType, program: SProgram): Option[SType] = (left, right) match {
    case (a, b) if a == b => Some(a)
    case (a: SInductiveType, b: SInductiveType) if a.name == b.name => Some(a)
    case (a: SInductiveType, b: SInductiveType) =>
      val actualData = findDataOrCase(a.name, program)
      val expectedData = findDataOrCase(b.name, program)
      (actualData, expectedData) match {
        case (Right(leftCase), Right(rightCase)) if leftCase == rightCase =>
          Some(a)
        case (Right(leftCase), Right(rightCase)) if leftCase.dataName == rightCase.dataName =>
          Some(SInductiveType(leftCase.dataName))
        case (Left(leftDataDecl), Left(rightDataDecl)) if leftDataDecl == rightDataDecl =>
          Some(a)
        case (Right(leftCase), Left(rightDataDecl)) if leftCase.dataName == rightDataDecl.name =>
          Some(b)
        case (Left(leftDataDecl), Right(rightCase)) if leftDataDecl.name == rightCase.dataName =>
          Some(a)
        case _ => None
      }
    case _ => None
  }

  private def isOfType(actual: SType, expected: SType, program: SProgram): Boolean = (actual, expected) match {
    case (a, b) if a == b => true
    case (a: SInductiveType, b: SInductiveType) if a.name == b.name => true
    case (a: SInductiveType, b: SInductiveType) =>
      val actualData = findDataOrCase(a.name, program)
      val expectedData = findDataOrCase(b.name, program)
      (actualData, expectedData) match {
        case (Right(actualCase), Right(expectedCase)) =>
          actualCase == expectedCase
        case (Left(actualDataDecl), Left(expectedDataDecl)) =>
          actualDataDecl == expectedDataDecl
        case (Right(actualCase), Left(expectedDataDecl)) =>
          actualCase.dataName == expectedDataDecl.name
        case _ => false
      }
    case _ => false
  }

  private def tryStringCast(left: SExpression, right: SExpression): (SExpression, SExpression) =
    (left.exprType, right.exprType) match {
      case (SStringType, SStringType) => (left, right)
      case (SStringType, _) => (left, StringCast(right))
      case (_, SStringType) => (StringCast(left), right)
      case _ => throw new RuntimeException("Cannot cast expressions")
    }

  override def visitExpression(expression: SExpression, data: TypeVisitorInfo): SExpression = {
    expression match {
      case SVariable(name, _) =>
        SVariable(name, data.variableTypes(name))
      case expr: DataConstructor =>
        val typedConstructor = super.visitExpression(expr, data).asInstanceOf[DataConstructor]
        typedConstructor.arguments.arguments.zip(expr.getDataCase(data.program).get.argTypes).foreach {
          case (e, p) => assume(isOfType(e.exprType, p, data.program), "Argument of data constructor of wrong type")
        }
        typedConstructor
      case expr: FunctionCallReference =>
        val typedCall = super.visitExpression(expr, data).asInstanceOf[FunctionCallReference]
        typedCall.arguments.arguments.zip(expr.getFunction(data.program).get.parameters.parameters).foreach {
          case (e, p) => assume(isOfType(e.exprType, p.parameterType, data.program), "Argument of function call of wrong type")
        }
        super.visitExpression(expression, data)
      case Addition(left, right, _) if left.exprType == SStringType || right.exprType == SStringType =>
        val (newLeft, newRight) = tryStringCast(visitExpression(left, data), visitExpression(right, data))
        StringConcatenation(newLeft, newRight)
      case expr: NumericBinaryOperation =>
        val (left, right) = (visitExpression(expr.left, data), visitExpression(expr.right, data))
        assume(left.exprType == right.exprType, "Using operation on incompatible types")
        assume(left.exprType.isInstanceOf[SNumericType], "Numeric operation with non numeric types")
        expr.newInstance(left, right, left.exprType)
      case expr: BooleanBinaryOperation =>
        val (left, right) = (visitExpression(expr.left, data), visitExpression(expr.right, data))
        assume(left.exprType == right.exprType && left.exprType == SBooleanType, "Boolean type expected")
        expr.newInstance(left, right)
      case expr: ComparisonOperation =>
        val (left, right) = (visitExpression(expr.left, data), visitExpression(expr.right, data))
        assume(left.exprType == right.exprType, "Using operation on incompatible types")
        assume(left.exprType.isInstanceOf[SNumericType], "Comparison of non numeric types")
        expr.newInstance(left, right, left.exprType)
      case _ => super.visitExpression(expression, data)
    }
  }

  override def visitStatement(statement: SStatement, data: TypeVisitorInfo): SStatement =
    statement match {
      case IfStatement(cond, trueList, falseList, t) =>
        val newTrueList = visitStatementList(trueList, data)
        val newFalseList = falseList.map(st => visitStatementList(st, data))
        val newExpression = visitExpression(cond, data)
        val statementType = if (newFalseList.isDefined)
          findCommonSuperType(newTrueList.statementType, newFalseList.get.statementType, data.program)
        else
          Some(SUnitType)
        assume(newExpression.exprType == SBooleanType, "If statement condition does not return boolean")
        assume(statementType.isDefined, "Failed to type if statement, could not find common super type of branches.")
        IfStatement(newExpression, newTrueList, newFalseList, statementType.get)
      case LetStatement(name, expr, Some(t)) =>
        val newExpr = visitExpression(expr, data)
        assume(isOfType(newExpr.exprType, t, data.program), "Let statement, type constraint could not be fulfilled.")
        data.variableTypes += name -> t
        LetStatement(name, expr, Some(t))
      case LetStatement(name, expr, None) =>
        val newExpr = visitExpression(expr, data)
        data.variableTypes += name -> newExpr.exprType
        LetStatement(name, expr, Some(newExpr.exprType))
      case s => super.visitStatement(statement, data)
    }

  override def visitFunction(function: SFunction, data: TypeVisitorInfo): SFunction = {
    val newData = TypeVisitorInfo(data.program, SNoTypeYet, function.parameters.parameters.foldLeft(Map.empty[String, SType]) {
      case (m, p) => m + (p.name -> p.parameterType)
    })
    val typedFunction = super.visitFunction(function, newData)
    data.variableTypes = Map.empty
    val lastType = typedFunction.body.statements match {
      case Nil => SUnitType
      case l =>
        l.last.statementType
    }
    if (!isOfType(lastType, typedFunction.returnType, data.program)) {
      throw new RuntimeException("Function is of wrong type")
    } else {
      typedFunction
    }
  }

  override def visitData(dataDeclaration: SData, data: TypeVisitorInfo): SData =
    super.visitData(dataDeclaration, data)

  override def visitDataCase(dataCase: SDataCase, data: TypeVisitorInfo): SDataCase = {
    super.visitDataCase(dataCase, data)
  }

  override def visitParameter(parameter: SParameter, data: TypeVisitorInfo): SParameter = {
    data.variableTypes += parameter.name -> parameter.parameterType
    parameter
  }


  override def visitProgram(program: SProgram, data: TypeVisitorInfo): SProgram =
    super.visitProgram(program, data)


  def visitProgram(program: SProgram): SProgram =
    super.visitProgram(program, TypeVisitorInfo(program, SNoTypeYet, Map.empty))

}
