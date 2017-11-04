package de.pylamo.visitors.irgeneration

import de.pylamo.language._
import de.pylamo.trees._
import de.pylamo.visitors.AbstractVisitor
import org.apache.bcel.Const
import org.apache.bcel.Const._
import org.apache.bcel.generic._

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

  private var jumpTargetMaps: List[scala.collection.mutable.Map[String, InstructionHandle]] = List(scala.collection.mutable.Map.empty)

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

  def putNewJumpTargetMap(): Unit = {
    jumpTargetMaps ::= scala.collection.mutable.Map.empty[String, InstructionHandle]
  }

  def popJumpTargetMap(): Unit = {
    jumpTargetMaps = jumpTargetMaps.tail
  }

  def defineJumpTarget(name: String): InstructionHandle = {
    defineJumpTarget(name, instructionList.append(new NOP))
  }

  def defineJumpTarget(name: String, handle: InstructionHandle): InstructionHandle = {
    jumpTargetMaps.head += (name -> handle)
    handle
  }

  def getJumpTarget(name: String): InstructionHandle = jumpTargetMaps.head(name)

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
      data.instructionList.append(goto)
      ifInstruction.setTarget(data.instructionList.append(new NOP()))
      visitStatementList(trueList, data)
      if (falseList.isEmpty) {
        data.instructionList.append(InstructionFactory.createPop(trueList.statementType.stackSize))
      }
      goto.setTarget(data.instructionList.append(new NOP()))
      if (falseList.isEmpty) {
        data.instructionList.append(loadUnit(data))
      }
      data.instructionList.append(new NOP)
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
    instructionList.append(InstructionFactory.createReturn(function.returnType.getBCELType))
    val methodGen = new MethodGen(ACC_STATIC, function.returnType.getBCELType, function.parameters.parameters.map(p => p.parameterType.getBCELType).toArray,
      null, function.name, "Program", instructionList, data.programClassGen.getConstantPool)
    methodGen.setMaxStack()
    methodGen.setMaxLocals()
    methodGen.removeNOPs()
    methodGen
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

  private def getLoadInstruction(varType: SType, localIndex: Int): Instruction =
    InstructionFactory.createLoad(varType.getBCELType, localIndex)

  private def storeInstruction(varType: SType, localIndex: Int): Instruction =
    InstructionFactory.createStore(varType.getBCELType, localIndex)

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
        constructorIL.append(factory.createPutField(dataCase.name, getFieldName(n), t.getBCELType))
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
    constructor.setMaxStack()
    constructor.setMaxLocals()

    val toStringIL = new InstructionList()
    toStringIL.append(factory.createNew("java.lang.StringBuffer"))
    toStringIL.append(new DUP())
    toStringIL.append(factory.createConstant(dataCase.name))
    toStringIL.append(factory.createInvoke("java.lang.StringBuffer", "<init>", Type.VOID, Array(Type.STRING), Const.INVOKESPECIAL))
    if (dataCase.argTypes.nonEmpty) {
      toStringIL.append(factory.createConstant("("))
      toStringIL.append(factory.createAppend(Type.STRING))
    }
    dataCase.argTypes.zipWithIndex.foreach {
      case (t, n) =>
        toStringIL.append(new ALOAD(0))
        toStringIL.append(factory.createGetField(dataCase.name, getFieldName(n), t.getBCELType))
        toStringIL.append(factory.createAppend(t.getBCELType))
        if (n < dataCase.argTypes.size - 1) {
          toStringIL.append(factory.createConstant(", "))
          toStringIL.append(factory.createAppend(Type.STRING))
        }
    }
    if (dataCase.argTypes.nonEmpty) {
      toStringIL.append(factory.createConstant(")"))
      toStringIL.append(factory.createAppend(Type.STRING))
    }
    toStringIL.append(factory.createInvoke("java.lang.StringBuffer", "toString", Type.STRING, Array(), Const.INVOKEVIRTUAL))
    toStringIL.append(new ARETURN)
    val toString = new MethodGen(ACC_PUBLIC,
      Type.STRING,
      Array(),
      Array(),
      "toString",
      dataCase.name,
      toStringIL,
      classGen.getConstantPool)
    toString.setMaxStack()
    toString.setMaxLocals()
    classGen.setMethods(Array(toString.getMethod, constructor.getMethod))
    classGen
  }

  def visitProgram(program: SProgram): List[ClassGen] = {
    val data = BytecodeVisitorData(program, "Program")
    visitProgram(program, data)
  }

  //Top of stack has the variable that is being matched against, this means that any match pattern has to make sure
  //it does not leave things on the stack!
  //TODO: Fix nested matches
  override def visitMatchPattern(pattern: MatchPattern,
                                 data: BytecodeVisitorData): Unit = pattern match {
    case Wildcard =>
      data.instructionList.append(getPopInstruction(data.getMatchType))
    case ConstructorPattern(name, subPatterns) =>
      val dataCase = data.program.findDataCase(name).get
      //Here we need nested jumps so we can clean stuff up
      val oldFalse = data.getJumpTarget("false")
      val newFalse = data.defineJumpTarget("false")
      data.instructionList.append(getDupInstruction(data.getMatchType))
      val goto = new GOTO(null)
      data.instructionList.append(goto)
      data.instructionList.move(newFalse, data.instructionList.append(new NOP()))
      val popHandle = data.instructionList.append(new POP())
      data.instructionList.append(new GOTO(oldFalse))
      goto.setTarget(data.instructionList.append(new NOP()))
      data.instructionList.append(data.instructionFactory.createInstanceOf(new ObjectType(dataCase.name)))
      data.instructionList.append(new IFEQ(popHandle))
      data.instructionList.append(data.instructionFactory.createCheckCast(new ObjectType(name)))
      subPatterns.zipWithIndex.foreach {
        case (p, n) =>
          data.instructionList.append(new DUP)
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
    data.instructionList.move(trueTarget, data.instructionList.append(new NOP()))
    data.instructionList.append(InstructionFactory.createPop(data.getMatchType.stackSize))
    visitStatementList(matchCase.statements, data)
    data.instructionList.append(new GOTO(endTarget))
    data.instructionList.move(falseTarget, data.instructionList.append(new NOP()))
  }

  override def visitMatchStatement(matchStatement: MatchStatement,
                                   data: BytecodeVisitorData): Unit = {
    data.addMatchType(matchStatement.expression.exprType)
    data.putNewJumpTargetMap()
    visitExpression(matchStatement.expression, data)
    val endTarget = data.defineJumpTarget("matchEnd")
    matchStatement.cases.foreach(c => visitMatchCase(c, data))
    val bcelType = matchStatement.expression.exprType.getBCELType
    val argType = if (bcelType.isInstanceOf[ObjectType]) Type.OBJECT else bcelType
    data.instructionList.append(data.instructionFactory.createInvoke("java.lang.String", "valueOf", Type.STRING, Array(argType), Const.INVOKESTATIC))
    data.instructionList.append(data.instructionFactory.createNew("java.lang.StringBuffer"))
    data.instructionList.append(new DUP())
    data.instructionList.append(data.instructionFactory.createConstant("Match failed, no pattern matched the supplied expression: "))
    data.instructionList.append(data.instructionFactory.createInvoke("java.lang.StringBuffer", "<init>", Type.VOID, Array(Type.STRING), Const.INVOKESPECIAL))
    data.instructionList.append(new SWAP())
    data.instructionList.append(data.instructionFactory.createAppend(Type.STRING))
    data.instructionList.append(data.instructionFactory.createInvoke("java.lang.StringBuffer", "toString", Type.STRING, Array(), Const.INVOKEVIRTUAL))

    data.instructionList.append(data.instructionFactory.createNew("java.lang.RuntimeException"))
    data.instructionList.append(new DUP_X1())
    data.instructionList.append(new DUP_X1())
    data.instructionList.append(new POP())
    data.instructionList.append(data.instructionFactory.createInvoke("java.lang.RuntimeException", "<init>", Type.VOID, Array(Type.STRING), Const.INVOKESPECIAL))
    data.instructionList.append(new ATHROW())
    data.instructionList.move(endTarget, data.instructionList.append(new NOP()))
    data.popMatchType()
    data.popJumpTargetMap()
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
    val staticInitMethod = new MethodGen(ACC_STATIC, Type.VOID, Array(), Array(), "<clinit>", "Unit", initIL, classGen.getConstantPool)
    staticInitMethod.setMaxStack()
    staticInitMethod.setMaxLocals()
    val initMethod = new MethodGen(ACC_PRIVATE, Type.VOID, Array(), Array(), "<linit>", "Unit", new InstructionList(new RETURN), classGen.getConstantPool)
    initMethod.setMaxStack()
    initMethod.setMaxLocals()
    classGen.setMethods(Array(initMethod.getMethod, staticInitMethod.getMethod))
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
    mainIL.append(new RETURN())
    val otherMethods = program.functions.map(f => visitFunction(f, data).getMethod)
    val mainMethod = new MethodGen(ACC_PUBLIC | ACC_STATIC, Type.VOID, Array(new ArrayType(Type.STRING, 1)), null, "main", "Program", mainIL, data.programClassGen.getConstantPool)
    mainMethod.setMaxLocals()
    mainMethod.setMaxStack()
    data.programClassGen.setMethods((mainMethod.getMethod :: otherMethods).toArray)
    getUnitClass :: data.programClassGen :: program.dataDeclarations.flatMap(d => visitData(d, data))
  }
}
