package de.pylamo.visitors

import de.pylamo.trees._

/**
  * Created by Fredy on 13.10.2017.
  */
class TreeBuildVisitor[E] extends AbstractVisitor[E, STree] {


  override def visitArgumentList(argumentList: ArgumentList, data: E): ArgumentList = {
    ArgumentList(argumentList.arguments.map(e => visitExpression(e, data)))
  }

  override def visitExpression(expression: SExpression, data: E): SExpression = expression match {
    case FunctionCallReference(name, args, t) =>
      FunctionCallReference(name, visitArgumentList(args, data), t)
    case expression@UnaryOperation(u) =>
      expression.newInstance(visitExpression(u, data))
    case expression@BinaryOperation(l, r) =>
      expression.newInstance(visitExpression(l, data), visitExpression(r, data))
    //These should be leafs
    case expr => expr

  }

  override def visitStatement(statement: SStatement, data: E): SStatement = statement match {
    case IfStatement(cond, trueList, falseList, t) =>
      val newTrueList = visitStatementList(trueList, data)
      val newFalseList = falseList.map(st => visitStatementList(st, data))
      IfStatement(visitExpression(cond, data), newTrueList, newFalseList, t)
    case LetStatement(name, expr, statementType) =>
      LetStatement(name, visitExpression(expr, data), statementType)
    case ExpressionStatement(expr) =>
      ExpressionStatement(visitExpression(expr, data))
  }

  override def visitStatementList(list: StatementList, data: E): StatementList = {
    StatementList(list.statements.map(s => visitStatement(s, data)))
  }

  override def visitFunction(function: SFunction, data: E): SFunction = {
    val newStatementList = visitStatementList(function.body, data)
    val newParameterList = visitParameterList(function.parameters, data)
    SFunction(function.name, function.returnType, newParameterList, newStatementList)
  }

  override def visitParameterList(parameterList: ParameterList, data: E): ParameterList = {
    ParameterList(parameterList.parameters.map(p => visitParameter(p, data)))
  }

  override def visitParameter(parameter: SParameter, data: E): SParameter = {
    parameter
  }

  override def visitData(dataDeclaration: SData, data: E): SData = {
    val newCases = dataDeclaration.cases.map(dc => visitDataCase(dc, data))
    SData(dataDeclaration.name, newCases)
  }

  override def visitDataCase(dataCase: SDataCase, data: E): SDataCase = {
    SDataCase(dataCase.name, dataCase.argTypes, dataCase.name)
  }

  override def visitProgram(program: SProgram, data: E): SProgram = {
    SProgram(program.functions.map(f => visitFunction(f, data)),
      program.dataDeclarations.map(d => visitData(d, data)))
  }
}
