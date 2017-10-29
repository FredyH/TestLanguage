package de.pylamo.visitors.irgeneration

import de.pylamo.language._
import de.pylamo.trees._
import de.pylamo.visitors.AbstractVisitor
import de.pylamo.visitors.semantics.SemanticsVisitor.visitArgumentList
import org.apache.bcel.Const
import org.apache.bcel.generic._

import scala.annotation.tailrec
import org.apache.bcel.Const.{IF_ICMPEQ, _}
import org.apache.bcel.classfile.JavaClass

import scala.language.postfixOps

/**
  * Created by Fredy on 15.10.2017.
  */

//TODO: Add scoping to variables, like in semantics visitor
case class BytecodeVisitorData(program: SProgram, className: String) {

  val programClassGen = new ClassGen("Program", "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER | ACC_STATIC, Array())
  val instructionFactory = new InstructionFactory(programClassGen)
  var instructionList = new InstructionList()

  private var variableMap: Map[String, Int] = Map.empty

  private var labelIndex = 0
  private var variableIndex = 0

  private var jumpTargetMap: Map[String, InstructionHandle] = Map.empty

  def addVariable(name: String, exprType: SType): Int = {
    val varIndex = nextVariableIndex(exprType.stackSize)
    variableMap += name -> varIndex
    varIndex
  }

  def resetVariables(): Unit = {
    variableMap = Map.empty
    variableIndex = 0
  }

  def getVariable(name: String): Int = variableMap(name)

  def defineJumpTarget(name: String): InstructionHandle = {
    defineJumpTarget(name, instructionList.getEnd)
  }

  def defineJumpTarget(name: String, handle: InstructionHandle): InstructionHandle = {
    jumpTargetMap += (name -> handle)
    handle
  }

  def getJumpTarget(name: String): InstructionHandle = jumpTargetMap(name)

  def nextLabelIndex(): Int = {
    labelIndex += 1
    labelIndex
  }

  def nextVariableIndex(incAmount: Int = 1): Int = {
    val returnIndex = variableIndex
    variableIndex += incAmount
    returnIndex
  }

  def resetVariableIndex(): Unit = {
    variableIndex = 0
  }

  private var matchDepth: Int = 0

  def increaseMatchDepth(): Int = {
    matchDepth += 1
    matchDepth
  }

  def decreaseMatchDepth(): Int = {
    matchDepth -= 1
    matchDepth
  }

  private var matchTypes: List[SType] = Nil

  def addMatchType(matchType: SType): Unit = {
    matchTypes = matchType :: matchTypes
  }

  def getMatchType: SType = matchTypes.head

  def popMatchType(): Unit = {
    matchTypes = matchTypes.tail
  }
}

//TODO: A lot of code duplication can be removed by writing function that returns correct instruction based on type

object BytecodeVisitor extends AbstractVisitor[BytecodeVisitorData, Any] {


  override def visitArgumentList(argumentList: ArgumentList, data: BytecodeVisitorData): Unit = {
    argumentList.arguments.foreach(e => visitExpression(e, data))
  }

  def visitBinaryOperation(operation: BinaryOperation, data: BytecodeVisitorData): Unit = {
    visitExpression(operation.left, data)
    visitExpression(operation.right, data)
    operation.appendBytecodeInstructions(data)
  }

  override def visitExpression(expression: SExpression,
                               data: BytecodeVisitorData): Unit = expression match {
    case expr@DataConstructor(name, args) =>
      data.instructionList.append(data.instructionFactory.createNew(name))
      data.instructionList.append(new DUP())
      visitArgumentList(args, data)
      data.instructionList.append(data.instructionFactory.createInvoke(expr.name, "<init>", Type.VOID, expr.getBCELArgumentTypes(data.program), Const.INVOKESPECIAL))
    case expr@FunctionCallReference(name, argumentList, exprType) =>
      visitArgumentList(argumentList, data)
      data.instructionList.append(data.instructionFactory.createInvoke(data.className, name, expr.getBCELReturnType, expr.getBCELArgumentTypes(data.program), Const.INVOKESTATIC))
    case expr@UnaryOperation(subExpr) =>
      visitExpression(subExpr, data)
      expr.appendBytecodeInstructions(data)
    case expr: BinaryOperation =>
      visitBinaryOperation(expr, data)
    case expr =>
      expr.appendBytecodeInstructions(data)
  }

  private def getDupInstruction(varType: SType) =
    if (varType.stackSize == 1)
      new DUP()
    else
      new DUP2()

  private def getPopInstruction(varType: SType) =
    if (varType.stackSize == 1)
      new POP()
    else
      new POP2()


  override def visitStatementList(statement: StatementList,
                                  data: BytecodeVisitorData): Unit = {
    val listLength = statement.statements.size
    statement.statements.zipWithIndex.foreach {
      case (s, n) =>
        //This makes sure that the stack is clean, i.e. no value is ever left on the stack unless it is returned from
        //the function (or the function right after)
        visitStatement(s, data)
        if (listLength != n + 1) {
          data.instructionList.append(getPopInstruction(s.statementType))
        }
    }
  }

  override def visitStatement(statement: SStatement,
                              data: BytecodeVisitorData): Unit = statement match {
    case LetStatement(name, expr, varType) =>
      data.addVariable(name, varType.get)
      visitExpression(expr, data)
      data.instructionList.append(storeInstruction(varType.get, data.getVariable(name)))
      loadUnit(data)
    case ExpressionStatement(expression) =>
      visitExpression(expression, data)
    case statement: MatchStatement =>
      visitMatchStatement(statement, data)
    case IfStatement(condition, trueList, falseList, ifType) =>
      val trueLabel = Symbol("trueLabel" + data.nextLabelIndex())
      val falseLabel = Symbol("falseLabel" + data.nextLabelIndex())
      visitExpression(condition, data)
      data.instructionList.append(new ICONST(1))
      val ifInstruction = new IF_ICMPEQ(null)
      data.instructionList.append(ifInstruction)
      falseList.foreach(l => visitStatementList(l, data))
      val goto = new GOTO(null)
      ifInstruction.setTarget(data.instructionList.append(new NOP()))
      visitStatementList(trueList, data)
      goto.setTarget(data.instructionList.append(new NOP()))
      if (falseList.isDefined) {
        data.instructionList.append(loadUnit(data))
      }
  }

  override def visitParameterList(parameterList: ParameterList, data: BytecodeVisitorData): Any = {
    parameterList.parameters.map(p => visitParameter(p, data))
  }

  override def visitFunction(function: SFunction, data: BytecodeVisitorData): MethodGen = {
    data.resetVariables()
    val instructionList = new InstructionList()
    data.instructionList = instructionList
    visitParameterList(function.parameters, data)
    visitStatementList(function.body, data)
    function.returnType match {
      case SIntType =>
        instructionList.append(new LRETURN())
      case SFloatType =>
        instructionList.append(new DRETURN())
      case SBooleanType =>
        instructionList.append(new IRETURN())
      case SInductiveType(_) =>
        instructionList.append(new ARETURN())
      case SStringType =>
        instructionList.append(new ARETURN())
      case SUnitType =>
        instructionList.append(new RETURN())
    }
    new MethodGen(ACC_STATIC, function.returnType.getBCELType, function.parameters.parameters.map(p => p.parameterType.getBCELType).toArray,
      null, function.name, "Program", instructionList, data.programClassGen.getConstantPool)
  }

  override def visitParameter(parameter: SParameter, data: BytecodeVisitorData): Any = {
    data.addVariable(parameter.name, parameter.parameterType)
  }


  override def visitData(dataDeclaration: SData, data: BytecodeVisitorData): List[ClassGen] = {
    val interface = new ClassGen(
      dataDeclaration.name,
      "java.lang.Object",
      "<generated>",
      ACC_INTERFACE,
      Array()
    )
    interface :: dataDeclaration.cases.map(dc => visitDataCase(dc, data))
  }

  private def getLoadInstruction(varType: SType, localIndex: Int): Instruction = varType match {
    case SInductiveType(name) =>
      new ALOAD(localIndex)
    case SBooleanType =>
      new ILOAD(localIndex)
    case SIntType =>
      new LLOAD(localIndex)
    case SStringType =>
      new ALOAD(localIndex)
    case SUnitType =>
      new ALOAD(localIndex)
    case SFloatType =>
      new DLOAD(localIndex)
    case _ =>
      throw new RuntimeException("undexpected type " + varType + " while trying to store variable")
  }

  private def storeInstruction(varType: SType, localIndex: Int): Instruction = varType match {
    case SInductiveType(name) =>
      new ASTORE(localIndex)
    case SBooleanType =>
      new ISTORE(localIndex)
    case SIntType =>
      new LSTORE(localIndex)
    case SStringType =>
      new ASTORE(localIndex)
    case SUnitType =>
      //TODO: unit is not done yet
      new ASTORE(localIndex)
    case SFloatType =>
      new DSTORE(localIndex)
    case _ =>
      throw new RuntimeException("undexpected type " + varType + " while trying to store variable")
  }

  private def storeObjectVariable(varType: SType,
                                  className: String,
                                  fieldName: String,
                                  data: BytecodeVisitorData): Instruction =
    data.instructionFactory.createPutField(className, fieldName, varType.getBCELType)

  private def getFieldName(index: Int) = "field" + index

  override def visitDataCase(dataCase: SDataCase, data: BytecodeVisitorData): ClassGen = {
    val classGen = new ClassGen(
      dataCase.name,
      "java.lang.Object",
      "<generated>",
      ACC_PUBLIC | ACC_SUPER,
      Array(data.program.findData(dataCase.dataName).get.name)
    )
    dataCase.argTypes.zipWithIndex.foreach {
      case (t, n) =>
        classGen.addField(new FieldGen(ACC_PUBLIC, t.getBCELType, getFieldName(n), classGen.getConstantPool).getField)
    }
    val factory = new InstructionFactory(classGen)
    val constructorIL = new InstructionList()
    constructorIL.append(new ALOAD(0))
    constructorIL.append(factory.createInvoke("java.lang.Object", "<init>", Type.VOID, Array(), Const.INVOKESPECIAL))
    var stackIndex = 1
    dataCase.argTypes.zipWithIndex.foreach {
      case (t, n) =>
        constructorIL.append(new ALOAD(0))
        constructorIL.append(getLoadInstruction(t, stackIndex))
        constructorIL.append(storeObjectVariable(t, dataCase.name, getFieldName(n), data))
        stackIndex = stackIndex + t.stackSize
    }
    constructorIL.append(new RETURN())

    val constructor = new MethodGen(ACC_PUBLIC,
      Type.VOID,
      dataCase.argTypes.map(_.getBCELType).toArray,
      dataCase.argTypes.zipWithIndex.map(p => "arg" + p._2).toArray,
      "<init>",
      dataCase.name,
      constructorIL,
      classGen.getConstantPool)

    val toStringIL = new InstructionList()
    toStringIL.append(factory.createNew("java.lang.StringBuffer"))
    toStringIL.append(factory.createConstant(dataCase.name))
    toStringIL.append(factory.createInvoke("java.lang.StringBuffer", "<init>", Type.VOID, Array(Type.STRING), Const.INVOKESPECIAL))
    if (dataCase.argTypes.nonEmpty) {
      toStringIL.append(factory.createConstant("("))
      toStringIL.append(factory.createAppend(Type.STRING))
    }
    dataCase.argTypes.zipWithIndex.foreach {
      case (t, n) =>
        if (n <= dataCase.argTypes.size - 1) {
          toStringIL.append(factory.createConstant(", "))
          toStringIL.append(factory.createAppend(Type.STRING))
        }
        toStringIL.append(new ALOAD(0))
        toStringIL.append(factory.createGetField(dataCase.name, getFieldName(n), t.getBCELType))
        toStringIL.append(factory.createAppend(t.getBCELType))
    }
    if (dataCase.argTypes.nonEmpty) {
      toStringIL.append(factory.createConstant(")"))
      toStringIL.append(factory.createAppend(Type.STRING))
    }
    val toString = new MethodGen(ACC_PUBLIC,
      Type.STRING,
      Array(),
      Array(),
      "toString",
      dataCase.name,
      toStringIL,
      classGen.getConstantPool)
    classGen.setMethods(Array(toString.getMethod, constructor.getMethod))
    classGen
  }

  def visitProgram(program: SProgram): List[ClassGen] = {
    val data = BytecodeVisitorData(program, "Program")
    visitProgram(program, data)
  }

  //Top of stack has the variable that is being matched against, this means that any match pattern has to make sure
  //it does not leave things on the stack!
  //TODO: Fix bugs with nested matches
  override def visitMatchPattern(pattern: MatchPattern,
                                 data: BytecodeVisitorData): Unit = pattern match {
    case Wildcard =>
      data.instructionList.append(getPopInstruction(data.getMatchType))
    case ConstructorPattern(name, subPatterns) =>
      val dataCase = data.program.findDataCase(name).get
      //Here we need nested jumps so we can clean stuff up
      val oldFalse = data.getJumpTarget("false")
      val newFalse = data.defineJumpTarget("false")
      val proceedLabel = data.defineJumpTarget("proceed")
      data.instructionList.append(getDupInstruction(data.getMatchType))
      data.instructionList.append(data.instructionFactory.createInstanceOf(new ObjectType(dataCase.name)))
      data.instructionList.append(new IFEQ(oldFalse))
      newFalse.setInstruction(data.instructionList.append(getPopInstruction(data.getMatchType)).getInstruction)
      data.instructionList.append(new GOTO(oldFalse))
      proceedLabel.setInstruction(data.instructionList.append(new NOP()).getInstruction)
      data.instructionList.append(data.instructionFactory.createCheckCast(new ObjectType(name)))
      subPatterns.zipWithIndex.foreach {
        case (p, n) =>
          val fieldName = getFieldName(n)
          val fieldType = dataCase.argTypes(n)
          data.addMatchType(fieldType)
          data.instructionList.append(data.instructionFactory.createGetField(name, fieldName, fieldType.getBCELType))
          visitMatchPattern(p, data)
          data.popMatchType()
      }
      data.instructionList.append(new POP)
    case a: ConstantExpression =>
      visitExpression(a, data)
      Equals.appendCmpInstruction(a.exprType, data)
      data.instructionList.append(new IFEQ(data.getJumpTarget("false")))
    case SVariable(name, exprType) =>
      data.addVariable(name, exprType)
      data.instructionList.append(storeInstruction(exprType, data.getVariable(name)))
  }

  override def visitMatchCase(matchCase: MatchCase,
                              data: BytecodeVisitorData): Unit = {
    //This target contains
    val endTarget = data.getJumpTarget("matchEnd")
    val trueTarget = data.defineJumpTarget("true")
    val falseTarget = data.defineJumpTarget("false")
    data.instructionList.append(getDupInstruction(data.getMatchType))
    visitMatchPattern(matchCase.pattern, data)
    trueTarget.setInstruction(data.instructionList.append(new NOP()).getInstruction)
    visitStatementList(matchCase.statements, data)
    data.instructionList.append(new GOTO(endTarget))
    falseTarget.setInstruction(data.instructionList.append(new NOP).getInstruction)
  }

  override def visitMatchStatement(matchStatement: MatchStatement,
                                   data: BytecodeVisitorData): Unit = {
    data.addMatchType(matchStatement.expression.exprType)
    visitExpression(matchStatement.expression, data)
    val endTarget = data.defineJumpTarget("matchEnd")
    matchStatement.cases.foreach(c => visitMatchCase(c, data))

    data.instructionList.append(data.instructionFactory.createNew("java/lang/RuntimeException"))
    data.instructionList.append(new DUP())
    data.instructionList.append(data.instructionFactory.createConstant("Match failed, no pattern matched the supplied expression"))
    data.instructionList.append(data.instructionFactory.createInvoke("java.lang.RuntimeException", "<init>", Type.VOID, Array(Type.STRING), Const.INVOKESPECIAL))
    data.instructionList.append(new ATHROW())
    endTarget.setInstruction(data.instructionList.append(new NOP).getInstruction)
    data.popMatchType()
  }


  private def loadUnit(data: BytecodeVisitorData): Instruction =
    data.instructionFactory.createGetStatic("Unit", "unit", new ObjectType("Unit"))

  private def getUnitClass = {
    val classGen = new ClassGen("Unit", "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_STATIC | ACC_SUPER, Array())
    val factory = new InstructionFactory(classGen.getConstantPool)
    val initIL = new InstructionList()
    initIL.append(factory.createNew("Unit"))
    initIL.append(new DUP())
    initIL.append(factory.createInvoke("Unit", "<init>", Type.VOID, Array(), Const.INVOKESPECIAL))
    initIL.append(factory.createPutStatic("Unit", "unit", new ObjectType("Unit")))
    val staticInitMethod = new MethodGen(ACC_STATIC, Type.VOID, Array(), Array(), "<clinit>", "Unit", initIL, classGen.getConstantPool).getMethod
    val initMethod = new MethodGen(ACC_PRIVATE, Type.VOID, Array(), Array(), "<linit>", "Unit", new InstructionList(new RETURN), classGen.getConstantPool).getMethod
    classGen.setMethods(Array(initMethod, staticInitMethod))
    classGen
  }

  override def visitProgram(program: SProgram, data: BytecodeVisitorData): List[ClassGen] = {
    val mainFunction = program.functions.find(_.name == "main").get
    val mainIL = new InstructionList()
    mainIL.append(data.instructionFactory.createGetStatic("java.lang.System", "out", new ObjectType("java.io.PrintStream")))
    mainIL.append(data.instructionFactory.createInvoke("Program",
      "main",
      mainFunction.returnType.getBCELType,
      mainFunction.parameters.parameters.map(_.parameterType.getBCELType).toArray, Const.INVOKESTATIC))
    mainFunction.returnType match {
      case SIntType =>
        mainIL.append(data.instructionFactory.createInvoke("java.io.PrintStream", "println", Type.VOID, Array(Type.LONG), Const.INVOKEVIRTUAL))
      case SFloatType =>
        mainIL.append(data.instructionFactory.createInvoke("java.io.PrintStream", "println", Type.VOID, Array(Type.FLOAT), Const.INVOKEVIRTUAL))
      case SBooleanType =>
        mainIL.append(data.instructionFactory.createInvoke("java.io.PrintStream", "println", Type.VOID, Array(Type.BOOLEAN), Const.INVOKEVIRTUAL))
      case _ =>
        mainIL.append(data.instructionFactory.createInvoke("java.io.PrintStream", "println", Type.VOID, Array(Type.OBJECT), Const.INVOKEVIRTUAL))
    }
    val otherMethods = program.functions.map(f => visitFunction(f, data).getMethod)
    val mainMethod = new MethodGen(ACC_PUBLIC | ACC_STATIC, Type.VOID, Array(new ArrayType(Type.STRING, 1)), null, "main", "Program", mainIL, data.programClassGen.getConstantPool)
    data.programClassGen.setMethods((mainMethod.getMethod :: otherMethods).toArray)
    getUnitClass :: data.programClassGen :: program.dataDeclarations.flatMap(d => visitData(d, data))
  }
}
