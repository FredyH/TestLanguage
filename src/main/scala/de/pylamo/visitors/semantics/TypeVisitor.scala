package de.pylamo.visitors.semantics

import de.pylamo.language._
import de.pylamo.trees._
import de.pylamo.visitors.TreeBuildVisitor

/**
  * Created by Fredy on 12.10.2017.
  */
case class TypeVisitorInfo(expectedType: SType, var variableTypes: Map[String, SType])

object TypeVisitor extends TreeBuildVisitor[TypeVisitorInfo] {

  private def tryCast(left: SExpression, right: SExpression): (SExpression, SExpression) =
    (left.exprType, right.exprType) match {
      case (SStringType, SStringType) => (left, right)
      case (SStringType, _) => (left, StringCast(right))
      case (_, SStringType) => (StringCast(left), right)
      case _ => tryCastNumeric(left, right)
    }

  private def tryCastNumeric(left: SExpression, right: SExpression): (SExpression, SExpression) =
    (left.exprType, right.exprType) match {
      case (SIntType, SIntType) => (left, right)
      case (SIntType, SFloatType) => (FloatCast(left), right)
      case (SFloatType, SIntType) => (left, FloatCast(right))
      case (SFloatType, SFloatType) => (left, right)
      case _ => throw new RuntimeException("Can't cast numerically")
    }

  override def visitExpression(expression: SExpression, data: TypeVisitorInfo): SExpression = {
    expression match {
      case SVariable(name, _) =>
        SVariable(name, data.variableTypes(name))
      case Addition(left, right, _) =>
        val (newLeft, newRight) = tryCast(visitExpression(left, data), visitExpression(right, data))
        Addition(newLeft, newRight, newLeft.exprType)
      case expr: NumericBinaryOperation =>
        val (left, right) = tryCastNumeric(visitExpression(expr.left, data), visitExpression(expr.right, data))
        expr.newInstance(left, right, left.exprType)
      case expr: BooleanBinaryOperation =>
        val (left, right) = (visitExpression(expr.left, data), visitExpression(expr.right, data))
        assume(left.exprType == right.exprType && left.exprType == SBooleanType, "Boolean type expected")
        expr.newInstance(left, right)
      case _ => super.visitExpression(expression, data)
    }
  }

  override def visitStatement(statement: SStatement, data: TypeVisitorInfo): SStatement =
    statement match {
      case IfStatement(cond, trueList, falseList, t) =>
        val newTrueList = visitStatementList(trueList, data)
        val newFalseList = falseList.map(st => visitStatementList(st, data))
        val newExpression = visitExpression(cond, data)
        assume(newExpression.exprType == SBooleanType, "If statement condition does not return boolean")
        //TODO: Correct typing
        IfStatement(newExpression, newTrueList, newFalseList, newTrueList.statementType)
      case LetStatement(name, expr, _) =>
        val newExpr = visitExpression(expr, data)
        data.variableTypes += name -> newExpr.exprType
        LetStatement(name, expr, expr.exprType)
      case s => super.visitStatement(statement, data)
    }

  override def visitFunction(function: SFunction, data: TypeVisitorInfo): SFunction = {
    val newData = TypeVisitorInfo(SNoTypeYet, function.parameters.parameters.foldLeft(Map.empty[String, SType]) {
      case (m, p) => m + (p.name -> p.parameterType)
    })
    val typedFunction = super.visitFunction(function, newData)
    data.variableTypes = Map.empty
    val lastType = typedFunction.body.statements match {
      case Nil => SUnitType
      case l =>
        l.last.statementType
    }
    if (lastType != typedFunction.returnType) {
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


  override def visitProgram(program: SProgram, data: TypeVisitorInfo = TypeVisitorInfo(SNoTypeYet, Map.empty)): SProgram =
    super.visitProgram(program, data)


}
