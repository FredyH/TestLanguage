package de.pylamo.trees

import de.pylamo.language.{SNoTypeYet, SType, SUnitType}


/**
  * Created by Fredy on 12.10.2017.
  */

case class StatementList(statements: List[SStatement]) extends STree {
  def statementType: SType =
    if (statements.isEmpty)
      SUnitType
    else
      statements.last.statementType
}

sealed trait SStatement extends STree {
  def statementType: SType = this match {
    case ExpressionStatement(expr) => expr.exprType
    case _ => SUnitType
  }
}


case class LetStatement(name: String, expr: SExpression, variableType: Option[SType]) extends SStatement {

}

case class ExpressionStatement(expr: SExpression) extends SStatement

case class IfStatement(condition: SExpression,
                       trueList: StatementList,
                       falseList: Option[StatementList] = None,
                       ifType: SType = SNoTypeYet) extends SStatement {
  override def statementType: SType = ifType
}