package de.pylamo.visitors.irgeneration

import de.pylamo.language._
import de.pylamo.trees._
import de.pylamo.visitors.AbstractVisitor
import de.pylamo.visitors.semantics.SemanticsVisitor.visitArgumentList
import org.opalj.ba._
import org.opalj.br.{FieldType, PC}
import org.opalj.br.instructions.{INVOKESPECIAL, _}
import org.opalj.da.ClassFile

import scala.language.postfixOps

/**
  * Created by Fredy on 15.10.2017.
  */

//TODO: Add scoping to variables, like in semantics visitor
case class BytecodeVisitorData(program: SProgram, className: String) {

  private var variableMap: Map[String, Int] = Map.empty

  private var labelIndex = 0
  private var variableIndex = 0

  private var jumpTargetMap: Map[String, Symbol] = Map.empty

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

  def defineJumpTarget(name: String): Symbol = {
    val symbol = Symbol(name + nextLabelIndex())
    jumpTargetMap += (name -> symbol)
    symbol
  }

  def getJumpTarget(name: String): Symbol = jumpTargetMap(name)

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
  type CodeList = List[CodeElement[InstructionElement]]

  private def CodeList(args: CodeElement[InstructionElement]*): CodeList = {
    List(args: _*)
  }


  override def visitArgumentList(argumentList: ArgumentList, data: BytecodeVisitorData): CodeList = {
    argumentList.arguments.flatMap(e => visitExpression(e, data))
  }

  def visitBinaryOperation(operation: BinaryOperation, data: BytecodeVisitorData): CodeList = {
    val leftCode = visitExpression(operation.left, data)
    val rightCode = visitExpression(operation.right, data)
    val otherCode = leftCode ++ rightCode
    otherCode ++ operation.getBytecodeInstructions(data)
  }

  override def visitExpression(expression: SExpression,
                               data: BytecodeVisitorData): CodeList = expression match {
    case expr@DataConstructor(name, args) =>
      val dataCase = expr.getDataCase(data.program).get
      val argsCode = visitArgumentList(args, data)
      CodeList(
        NEW(name),
        DUP
      ) ++ argsCode ++ CodeList(
        INVOKESPECIAL(dataCase.name, isInterface = false, "<init>", expr.getJavaMethodDescriptor(data.program))
      )
    case expr@FunctionCallReference(name, argumentList, exprType) =>
      val argsCode = visitArgumentList(argumentList, data)
      argsCode ++ CodeList(INVOKESTATIC(data.className, false, name, expr.getJavaMethodDescriptor(data.program)))
    case expr@UnaryOperation(subExpr) =>
      visitExpression(subExpr, data) ++ expr.getBytecodeInstructions(data)
    case expr: BinaryOperation =>
      visitBinaryOperation(expr, data)
    case expr =>
      expr.getBytecodeInstructions(data)
  }

  private def getDupInstruction(varType: SType) =
    if (varType.stackSize == 1)
      DUP
    else
      DUP2

  private def getPopInstruction(varType: SType) =
    if (varType.stackSize == 1)
      POP
    else
      POP2


  override def visitStatementList(statement: StatementList,
                                  data: BytecodeVisitorData): CodeList = {
    val listLength = statement.statements.size
    statement.statements.zipWithIndex.flatMap {
      case (s, n) =>
        //This makes sure that the stack is clean, i.e. no value is ever left on the stack unless it is returned from
        //the function (or the function right after)
        val popInstruction = if (listLength == n + 1) CodeList() else CodeList(getPopInstruction(s.statementType))
        visitStatement(s, data) ++ popInstruction
    }
  }

  override def visitStatement(statement: SStatement,
                              data: BytecodeVisitorData): CodeList = statement match {
    case LetStatement(name, expr, varType) =>
      data.addVariable(name, varType.get)
      val expressionCode = visitExpression(expr, data)
      val storeInstr = storeInstruction(varType.get, data.getVariable(name))
      expressionCode ++ CodeList(storeInstr) ++ loadUnit
    case ExpressionStatement(expression) =>
      visitExpression(expression, data)
    case statement: MatchStatement =>
      visitMatchStatement(statement, data)
    case IfStatement(condition, trueList, falseList, ifType) =>
      val trueLabel = Symbol("trueLabel" + data.nextLabelIndex())
      val falseLabel = Symbol("falseLabel" + data.nextLabelIndex())
      val condCode = visitExpression(condition, data)
      val trueCode = visitStatementList(trueList, data)
      val falseCode = falseList match {
        case Some(list) => visitStatementList(list, data)
        case None => CodeList()
      }
      val endLoadCode = falseList match {
        case Some(_) => CodeList()
        case None => loadUnit
      }
      condCode ++ CodeList(ICONST_1, IF_ICMPEQ(trueLabel)) ++ falseCode ++
        CodeList(GOTO(falseLabel), trueLabel) ++ trueCode ++ CodeList(falseLabel) ++ endLoadCode
  }

  override def visitParameterList(parameterList: ParameterList, data: BytecodeVisitorData): Any = {
    parameterList.parameters.map(p => visitParameter(p, data))
  }

  override def visitFunction(function: SFunction, data: BytecodeVisitorData): METHOD[(Map[PC, InstructionElement], List[String])] = {
    data.resetVariables()
    visitParameterList(function.parameters, data)
    val code = visitStatementList(function.body, data)
    val returnCode = function.body.statementType match {
      case SIntType =>
        CodeList(LRETURN)
      case SFloatType =>
        CodeList(DRETURN)
      case SBooleanType =>
        CodeList(IRETURN)
      case SInductiveType(_) =>
        CodeList(ARETURN)
      case SStringType =>
        CodeList(ARETURN)
    }
    METHOD(PUBLIC STATIC, function.name, function.getJavaMethodDescriptor, CODE(code ++ returnCode: _*).MAXLOCALS(data.nextVariableIndex()))
  }

  override def visitParameter(parameter: SParameter, data: BytecodeVisitorData): Any = {
    data.addVariable(parameter.name, parameter.parameterType)
  }


  override def visitData(dataDeclaration: SData, data: BytecodeVisitorData): List[ClassFile] = {
    val interface = CLASS(
      accessModifiers = INTERFACE ABSTRACT,
      thisType = dataDeclaration.name
    )
    val subClassFiles = dataDeclaration.cases.map(c => visitDataCase(c, data).toDA()._1)
    interface.toDA()._1 :: subClassFiles
  }

  private def getLoadInstruction(varType: SType, localIndex: Int): InstructionElement = varType match {
    case SInductiveType(name) =>
      ALOAD(localIndex)
    case SBooleanType =>
      ILOAD(localIndex)
    case SIntType =>
      LLOAD(localIndex)
    case SStringType =>
      ALOAD(localIndex)
    case SUnitType =>
      ALOAD(localIndex)
    case SFloatType =>
      DLOAD(localIndex)
    case _ =>
      throw new RuntimeException("undexpected type " + varType + " while trying to store variable")
  }

  private def storeInstruction(varType: SType, localIndex: Int): InstructionElement = varType match {
    case SInductiveType(name) =>
      ASTORE(localIndex)
    case SBooleanType =>
      ISTORE(localIndex)
    case SIntType =>
      LSTORE(localIndex)
    case SStringType =>
      ASTORE(localIndex)
    case SUnitType =>
      //TODO: unit is not done yet
      ASTORE(localIndex)
    case SFloatType =>
      DSTORE(localIndex)
    case _ =>
      throw new RuntimeException("undexpected type " + varType + " while trying to store variable")
  }

  private def storeObjectVariable(varType: SType, className: String, fieldName: String): List[InstructionElement] = varType match {
    case SInductiveType(name) =>
      List(PUTFIELD(className, fieldName, s"L$name;"))
    case SBooleanType =>
      List(PUTFIELD(className, fieldName, "Z"))
    case SIntType =>
      List(PUTFIELD(className, fieldName, "J"))
    case SStringType =>
      List(PUTFIELD(className, fieldName, "L/java/lang/String;"))
    case SUnitType =>
      //Putting unit does not make sense
      List()
    case SFloatType =>
      List(PUTFIELD(className, fieldName, "D"))
    case SNoTypeYet =>
      throw new RuntimeException("undexpected type " + varType + " while trying to store variable")
  }

  private def getFieldName(index: Int) = "field" + index

  override def visitDataCase(dataCase: SDataCase, data: BytecodeVisitorData): CLASS[(Map[PC, InstructionElement], List[String])] = {
    val fields = dataCase.argTypes.zipWithIndex.map {
      case (t, n) =>
        FIELD(
          accessModifiers = PUBLIC,
          name = getFieldName(n),
          descriptor = t.fieldDescriptor
        )
    }
    val superCode = CodeList(
      ALOAD_0,
      INVOKESPECIAL("java/lang/Object", isInterface = false, "<init>", "()V")
    )
    val assignmentCode = dataCase.argTypes.zipWithIndex.foldLeft((1, List.empty[CodeElement[InstructionElement]])) {
      case ((stackIndex, l), (t, n)) =>
        val loadInstructions = List[InstructionElement](ALOAD_0, getLoadInstruction(t, stackIndex))
        (stackIndex + t.stackSize, l ++ loadInstructions ++ storeObjectVariable(t, dataCase.name, getFieldName(n)))
    }._2
    val descriptorParams = dataCase.argTypes.flatMap(t => t.fieldDescriptor).mkString
    val constructor = METHOD(
      accessModifiers = PUBLIC,
      name = "<init>",
      descriptor = s"($descriptorParams)V",
      CODE(superCode ++ assignmentCode :+ CodeElement.instructionToInstructionElement(RETURN): _*)
    )

    val appendFieldCode = dataCase.argTypes.zipWithIndex.flatMap {
      case ((t, n)) =>
        val fieldDescriptor = if (t.isInstanceOf[SInductiveType]) "Ljava/lang/Object;" else t.fieldDescriptor
        val commaCode = if (n >= dataCase.argTypes.size - 1)
          List.empty[CodeElement[InstructionElement]]
        else
          CodeList(
            LoadString(", "),
            INVOKEVIRTUAL("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
          )
        CodeList(
          ALOAD_0,
          GETFIELD(dataCase.name, getFieldName(n), t.fieldDescriptor),
          INVOKEVIRTUAL("java/lang/StringBuilder", "append", s"($fieldDescriptor)Ljava/lang/StringBuilder;")
        ) ++ commaCode
    }

    val parenStartCode = if (dataCase.argTypes.isEmpty)
      CodeList()
    else
      CodeList(
        LoadString("("),
        INVOKEVIRTUAL("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
      )

    val parenEndCode = if (dataCase.argTypes.isEmpty)
      CodeList()
    else
      CodeList(
        LoadString(")"),
        INVOKEVIRTUAL("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
      )

    val toStringCode = CodeList(
      NEW("java/lang/StringBuilder"),
      DUP,
      LoadString(dataCase.name),
      INVOKESPECIAL("java/lang/StringBuilder", isInterface = false, "<init>", "(Ljava/lang/String;)V")
    ) ++ parenStartCode ++ appendFieldCode ++ parenEndCode ++ CodeList(
      INVOKEVIRTUAL("java/lang/Object", "toString", "()Ljava/lang/String;"),
      ARETURN
    )

    val toString = METHOD(
      accessModifiers = PUBLIC,
      name = "toString",
      descriptor = "()Ljava/lang/String;",
      code = CODE(toStringCode: _*)
    )
    CLASS(
      accessModifiers = PUBLIC SUPER,
      thisType = dataCase.name,
      fields = FIELDS(fields: _*),
      interfaceTypes = List(dataCase.dataName),
      methods = METHODS(constructor, toString)
    )
  }

  def visitProgram(program: SProgram): List[ClassFile] = {
    val data = BytecodeVisitorData(program, "Program")
    visitProgram(program, data)
  }

  //Top of stack has the variable that is being matched against, this means that any match pattern has to make sure
  //it does not leave things on the stack!
  //TODO: Fix bugs with nested matches
  override def visitMatchPattern(pattern: MatchPattern,
                                 data: BytecodeVisitorData): CodeList = pattern match {
    case Wildcard =>
      List(getPopInstruction(data.getMatchType))
    case ConstructorPattern(name, subPatterns) =>
      val dataCase = data.program.findDataCase(name).get
      //Here we need nested jumps so we can clean stuff up
      val oldFalse = data.getJumpTarget("false")
      val newFalse = data.defineJumpTarget("false")
      val proceedLabel = data.defineJumpTarget("proceed")
      val startList = CodeList(
        getDupInstruction(data.getMatchType),
        INSTANCEOF(name),
        IFNE(proceedLabel),
        newFalse,
        getPopInstruction(data.getMatchType),
        GOTO(oldFalse),
        proceedLabel,
        CHECKCAST(name)
      )
      val endList = CodeList(POP)
      val subPatternCode = subPatterns.zipWithIndex.flatMap {
        case (p, n) =>
          val fieldName = getFieldName(n)
          val fieldType = dataCase.argTypes(n)
          data.addMatchType(fieldType)
          val result = CodeList(GETFIELD(name, fieldName, fieldType.fieldDescriptor)) ++ visitMatchPattern(p, data)
          data.popMatchType()
          result
      }
      startList ++ subPatternCode ++ endList
    case a: ConstantExpression =>
      val loadInstruction = visitExpression(a, data)
      val cmpInstructions = Equals.getCmpInstructions(a.exprType, data)
      val jumpInstructions = CodeList(
        IFEQ(data.getJumpTarget("false"))
      )
      loadInstruction ++ cmpInstructions ++ jumpInstructions
    case SVariable(name, exprType) =>
      data.addVariable(name, exprType)
      CodeList(storeInstruction(exprType, data.getVariable(name)))
  }

  override def visitMatchCase(matchCase: MatchCase,
                              data: BytecodeVisitorData): CodeList = {
    //This target contains
    val gotoEnd: CodeList = List(GOTO(data.getJumpTarget("matchEnd")))
    val trueTarget: CodeList = List(data.defineJumpTarget("true"))
    val falseTarget: CodeList = List(data.defineJumpTarget("false"))
    val patternCode = visitMatchPattern(matchCase.pattern, data)
    val executeCode = visitStatementList(matchCase.statements, data)
    CodeList(getDupInstruction(data.getMatchType)) ++ patternCode ++ trueTarget ++ executeCode ++ gotoEnd ++ falseTarget
  }

  override def visitMatchStatement(matchStatement: MatchStatement,
                                   data: BytecodeVisitorData): CodeList = {
    data.addMatchType(matchStatement.expression.exprType)
    val expressionCode = visitExpression(matchStatement.expression, data)
    val endTarget: CodeList = CodeList(
      data.defineJumpTarget("matchEnd")
    )
    val casesCode = matchStatement.cases.flatMap(c => visitMatchCase(c, data))
    val errorCode: CodeList = CodeList(
      NEW("java/lang/RuntimeException"),
      DUP,
      LoadString("Match failed, no pattern matched the supplied expression"),
      INVOKESPECIAL("java/lang/RuntimeException", isInterface = false, "<init>", "(Ljava/lang/String;)V"),
      ATHROW
    )
    val result = expressionCode ++ casesCode ++ errorCode ++ endTarget
    data.popMatchType()
    result
  }


  private def loadUnit: CodeList = CodeList(
    GETSTATIC("Unit", "unit", "LUnit;")
  )

  private def getUnitClass = {
    val initCode = CODE(
      NEW("Unit"),
      DUP,
      INVOKESPECIAL("Unit", isInterface = false, "<init>", "()V"),
      PUTSTATIC("Unit", "unit", "LUnit;")
    )
    CLASS(
      accessModifiers = PUBLIC FINAL,
      thisType = "Unit",
      fields = FIELDS(FIELD(PUBLIC.FINAL.STATIC, "unit", "LUnit;")),
      methods = METHODS(
        METHOD(
          accessModifiers = STATIC,
          name = "<clinit>",
          descriptor = "()V",
          code = initCode
        ),
        METHOD(accessModifiers = PRIVATE, name = "<init>", descriptor = "()V")
      )
    )
  }

  override def visitProgram(program: SProgram, data: BytecodeVisitorData): List[ClassFile] = {
    val mainFunction = program.functions.find(_.name == "main").get
    val methods = program.functions.map(f => visitFunction(f, data)) :+
      METHOD(PUBLIC STATIC, "main", "([Ljava/lang/String;)V", CODE[InstructionElement](
        GETSTATIC("java/lang/System", "out", "Ljava/io/PrintStream;"),
        INVOKESTATIC(data.className, isInterface = false, "main", mainFunction.getJavaMethodDescriptor),
        mainFunction.returnType match {
          case SIntType =>
            INVOKEVIRTUAL("java/io/PrintStream", "println", "(J)V")
          case SFloatType =>
            INVOKEVIRTUAL("java/io/PrintStream", "println", "(D)V")
          case SBooleanType =>
            INVOKEVIRTUAL("java/io/PrintStream", "println", "(Z)V")
          case _ =>
            INVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/Object;)V")
        },
        RETURN
      ))
    val programClass = CLASS(
      accessModifiers = PUBLIC STATIC,
      thisType = data.className,
      methods = METHODS(methods: _*)
    )
    getUnitClass.toDA()._1 :: programClass.toDA()._1 :: program.dataDeclarations.flatMap(d => visitData(d, data))
  }
}
