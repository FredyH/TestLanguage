package de.pylamo.visitors.semantics

import de.pylamo.trees._
import de.pylamo.visitors.TreeBuildVisitor
import de.pylamo.visitors.semantics.TypeVisitor.visitArgumentList


/**
  * Created by Fredy on 13.10.2017.
  */

case class SemanticsVisitorInfo(program: SProgram) {
  private var variables: List[Set[String]] = List(Set.empty)

  def addVariable(name: String): Unit = {
    variables = (variables.head  + name) :: variables.tail
  }

  def variableExists(name: String): Boolean = {
    variables.exists(_.contains(name))
  }

  def enterScope(): Unit = variables = Set.empty[String] :: variables

  def leaveScope(): Unit = variables = variables.tail

}

//This visitor mainly deals with making sure that variables exist in the scope, no two variables
//are defined in the same scope, function calls refer to existing functions, etc.
object SemanticsVisitor extends TreeBuildVisitor[SemanticsVisitorInfo] {
  override def visitExpression(expression: SExpression, data: SemanticsVisitorInfo): SExpression = expression match {
    case FunctionCallReference(name, argumentList, _) if data.program.findDataCase(name).isDefined =>
      val constructor = data.program.findDataCase(name).get
      assume(constructor.argTypes.size == argumentList.arguments.size, "Data constructor supplied invalid amount of arguments")
      DataConstructor(name, visitArgumentList(argumentList, data))
    case FunctionCallReference(name, argumentList, _) =>
      val functionOpt = data.program.functions.find(_.name == name)
      assume(functionOpt.isDefined, s"Function $name not found")
      val Some(function) = functionOpt
      assume(function.parameters.parameters.size == argumentList.arguments.size, s"Wrong number of arguments for function $name")
      FunctionCallReference(name, visitArgumentList(argumentList, data), function.returnType)
    case SVariable(name, _) if data.program.findDataCase(name).isDefined =>
      val constructor = data.program.findDataCase(name).get
      assume(constructor.argTypes.isEmpty, "Data constructor with 0 elements with arguments found.")
      DataConstructor(name, ArgumentList(Nil))
    case SVariable(name, exprType) =>
      assume(data.variableExists(name), s"Variable with name $name not found in scope")
      expression
    case _ => super.visitExpression(expression, data)
  }

  //TODO: Remove variables after scoped statement!
  override def visitStatement(statement: SStatement, data: SemanticsVisitorInfo): SStatement = statement match {
    case LetStatement(name, expr, variableType) =>
      data.addVariable(name)
      super.visitStatement(statement, data)
    case _: ScopedStatement =>
      data.enterScope()
      val result = super.visitStatement(statement, data)
      data.leaveScope()
      result
    case _ => super.visitStatement(statement, data)
  }

  override def visitFunction(function: SFunction, data: SemanticsVisitorInfo): SFunction = {
    data.enterScope()
    function.parameters.parameters.foreach(p => data.addVariable(p.name))
    val result = super.visitFunction(function, data)
    data.leaveScope()
    result
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
    visitProgram(program, SemanticsVisitorInfo(program))
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


  override def visitMatchCase(matchCase: MatchCase, data: SemanticsVisitorInfo): MatchCase = {
    data.enterScope()
    val result = super.visitMatchCase(matchCase, data)
    data.leaveScope()
    result
  }

  //Converts function calls to data constructors,
  override def visitMatchPattern(pattern: MatchPattern, data: SemanticsVisitorInfo): MatchPattern = {
    pattern match {
      case _: ConstantExpression =>
        pattern
      case SVariable(name, exprType) if data.program.findDataOrCase(name).isDefined =>
        ConstructorPattern(name, Nil)
        //No typing done here yet
      case SVariable(name, exprType) =>
        data.addVariable(name)
        pattern
      case Wildcard =>
        Wildcard
      case ConstructorPattern(name, subExpressions) =>
        val dataCase = data.program.findDataCase(name)
        assume(dataCase.isDefined, "Match pattern with unknown data constructor")
        assume(subExpressions.size == dataCase.get.argTypes.size, "Invalid number of arguments for match pattern")
        ConstructorPattern(name, subExpressions.map(e => visitMatchPattern(e, data)))
      case _ => throw new RuntimeException(s"Unexpected match pattern expression: $pattern")
    }
  }
}
