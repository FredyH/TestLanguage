package de.pylamo.visitors.semantics

import de.pylamo.trees._
import de.pylamo.visitors.TreeBuildVisitor

/**
  * Created by Fredy on 13.10.2017.
  */

case class SemanticsVisitorInfo(program: SProgram, var variables: Set[String], constructors: Map[String, SDataCase]) {

}

//This visitor mainly deals with making sure that variables exist in the scope, no two variables
//are defined in the same scope, function calls refer to existing functions, etc.
object SemanticsVisitor extends TreeBuildVisitor[SemanticsVisitorInfo] {
  override def visitExpression(expression: SExpression, data: SemanticsVisitorInfo): SExpression = expression match {
    case FunctionCallReference(name, argumentList, _) if data.constructors.contains(name) =>
      val constructor = data.constructors(name)
      assume(constructor.argTypes.isEmpty, "Data constructor supplied invalid amount of arguments")
      DataConstructor(name, visitArgumentList(argumentList, data))
    case FunctionCallReference(name, argumentList, _) =>
      val functionOpt = data.program.functions.find(_.name == name)
      assume(functionOpt.isDefined, s"Function $name not found")
      val Some(function) = functionOpt
      assume(function.parameters.parameters.size == argumentList.arguments.size, s"Wrong number of arguments for function $name")
      FunctionCallReference(name, visitArgumentList(argumentList, data), function.returnType)
    case SVariable(name, _) if data.constructors.contains(name) =>
      val constructor = data.constructors(name)
      assume(constructor.argTypes.isEmpty, "Data constructor with 0 elements with arguments found.")
      DataConstructor(name, ArgumentList(Nil))
    case SVariable(name, exprType) =>
      assume(data.variables(name), s"Variable with name $name not found in scope")
      expression
    case _ => super.visitExpression(expression, data)
  }


  override def visitStatement(statement: SStatement, data: SemanticsVisitorInfo): SStatement = statement match {
    case LetStatement(name, expr, variableType) =>
      data.variables += name
      super.visitStatement(statement, data)
    case _ => super.visitStatement(statement, data)
  }

  override def visitFunction(function: SFunction, data: SemanticsVisitorInfo): SFunction = {
    data.variables = function.parameters.parameters.foldLeft(Set.empty[String]) {
      case (m, p) => m + p.name
    }
    super.visitFunction(function, data)
  }

  override def visitProgram(program: SProgram, data: SemanticsVisitorInfo): SProgram = {
    val functionNames = program.functions.map(_.name).toSet
    val dataNames = program.dataDeclarations.map(_.name).toSet
    val dataConstructorNames = program.dataDeclarations.flatMap(_.cases).map(_.name).toSet
    assume((functionNames ++ dataNames ++ dataConstructorNames).size == functionNames.size + dataNames.size + dataConstructorNames.size,
      "Duplicate name of function or data type found")
    assume(program.functions.groupBy(f => f.name).size == program.functions.size, "Functions with duplicate name found")
    assume(program.functions.exists(f => f.name == "main" && f.parameters.parameters.isEmpty), "No main function found")
    assume(program.dataDeclarations.groupBy(_.name).size == program.dataDeclarations.size, "Data declaration with duplicate names found")
    super.visitProgram(program, data)
  }

  def visitProgram(program: SProgram): SProgram = {
    val constructorNames = program.dataDeclarations.flatMap(_.cases).foldLeft(Map.empty[String, SDataCase]) {
      case (m, c) => m + (c.name -> c)
    }
    visitProgram(program, SemanticsVisitorInfo(program, Set(), constructorNames))
  }

  override def visitData(dataDeclaration: SData, data: SemanticsVisitorInfo): SData = {
    //TODO: Check data types can actually be instantiated maybe?
    //TODO: Make sure data types aren't keywords
    super.visitData(dataDeclaration, data)
  }

  override def visitDataCase(dataCase: SDataCase, data: SemanticsVisitorInfo): SDataCase = {
    val dataType = data.program.dataDeclarations.find(_.cases.exists(_.name == dataCase.name))
    assume(dataType.isDefined, "Did not find data declaration for data case" + dataCase.name)
    SDataCase(dataCase.name, dataCase.argTypes, dataType.get.name)
  }
}
