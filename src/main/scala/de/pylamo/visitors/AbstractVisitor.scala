package de.pylamo.visitors

import de.pylamo.trees._

/**
  * Created by Fredy on 13.10.2017.
  */
abstract class AbstractVisitor[E, +R] {

  def assume(b: Boolean, reason: String = "Assumption failed"): Unit = if (b) {
    ()
  } else {
    throw new RuntimeException(reason)
  }

  def visitArgumentList(argumentList: ArgumentList, data: E): R

  def visitExpression(expression: SExpression, data: E): R

  def visitStatementList(statement: StatementList, data: E): R

  def visitStatement(statement: SStatement, data: E): R

  def visitParameterList(parameterList: ParameterList, data: E): R

  def visitFunction(function: SFunction, data: E): R

  def visitParameter(parameter: SParameter, data: E): R

  def visitProgram(program: SProgram, data: E): R

  def visitData(dataDeclaration: SData, data: E): R

  def visitDataCase(dataCase: SDataCase, data: E): R

}
