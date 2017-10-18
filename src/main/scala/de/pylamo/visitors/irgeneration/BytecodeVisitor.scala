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
case class BytecodeVisitorData(program: SProgram, className: String, var variableMap: Map[String, Int]) {
  private var labelIndex = 0
  private var variableIndex = 0

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
}

//TODO: A lot of code duplication can be removed by writing function that returns correct instruction based on type

object BytecodeVisitor extends AbstractVisitor[BytecodeVisitorData, Any] {
  override def visitArgumentList(argumentList: ArgumentList, data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    argumentList.arguments.flatMap(e => visitExpression(e, data))
  }

  def visitBinaryOperation(operation: BinaryOperation, data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    val leftCode = visitExpression(operation.left, data)
    val rightCode = visitExpression(operation.right, data)
    val otherCode = leftCode ++ rightCode
    otherCode ++ operation.getBytecodeInstructions(data)
  }

  override def visitExpression(expression: SExpression,
                               data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = expression match {
    case expr@DataConstructor(name, args) =>
      val dataCase = expr.getDataCase(data.program).get
      val argsCode = visitArgumentList(args, data)
      List[CodeElement[InstructionElement]](
        NEW(name),
        DUP
      ) ++ argsCode ++ List[CodeElement[InstructionElement]](
        INVOKESPECIAL(dataCase.name, isInterface = false, "<init>", expr.getJavaMethodDescriptor(data.program))
      )
    case expr@FunctionCallReference(name, argumentList, exprType) =>
      val argsCode = visitArgumentList(argumentList, data)
      argsCode ++ List[CodeElement[InstructionElement]](INVOKESTATIC(data.className, false, name, expr.getJavaMethodDescriptor(data.program)))
    case expr@UnaryOperation(subExpr) =>
      visitExpression(subExpr, data) ++ expr.getBytecodeInstructions(data)
    case expr: BinaryOperation =>
      visitBinaryOperation(expr, data)
    case expr =>
      expr.getBytecodeInstructions(data)
  }

  override def visitStatementList(statement: StatementList,
                                  data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    statement.statements.flatMap(s => visitStatement(s, data))
  }

  override def visitStatement(statement: SStatement,
                              data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = statement match {
    case LetStatement(name, expr, _) =>
      val varIndex = data.nextVariableIndex()
      data.variableMap += (name -> varIndex)
      val expressionCode = visitExpression(expr, data)
      val storeExpression = expr.exprType match {
        case SBooleanType => List[CodeElement[InstructionElement]](ISTORE(varIndex))
        case SIntType => List[CodeElement[InstructionElement]](LSTORE(varIndex))
        case SStringType => List[CodeElement[InstructionElement]](ASTORE(varIndex))
        case SFloatType => List[CodeElement[InstructionElement]](DSTORE(varIndex))
        case SInductiveType(_) => List[CodeElement[InstructionElement]](ASTORE(varIndex))
      }
      expressionCode ++ storeExpression
    case ExpressionStatement(expression) =>
      visitExpression(expression, data)
    case IfStatement(condition, trueList, falseList, ifType) =>
      val trueLabel = Symbol("trueLabel" + data.nextLabelIndex())
      val falseLabel = Symbol("falseLabel" + data.nextLabelIndex())
      val condCode = visitExpression(condition, data)
      val trueCode = visitStatementList(trueList, data)
      val falseCode = falseList match {
        case Some(list) => visitStatementList(list, data)
        case None => List[CodeElement[InstructionElement]]()
      }
      condCode ++ List[CodeElement[InstructionElement]](ICONST_1, IF_ICMPEQ(trueLabel)) ++ falseCode ++
        List[CodeElement[InstructionElement]](GOTO(falseLabel), trueLabel) ++ trueCode ++ List[CodeElement[InstructionElement]](falseLabel)
  }

  override def visitParameterList(parameterList: ParameterList, data: BytecodeVisitorData): Any = {
    parameterList.parameters.map(p => visitParameter(p, data))
  }

  override def visitFunction(function: SFunction, data: BytecodeVisitorData): METHOD[(Map[PC, InstructionElement], List[String])] = {
    data.variableMap = Map()
    data.resetVariableIndex()
    visitParameterList(function.parameters, data)
    val code = visitStatementList(function.body, data)
    val returnCode = function.body.statementType match {
      case SIntType =>
        List[CodeElement[InstructionElement]](LRETURN)
      case SFloatType =>
        List[CodeElement[InstructionElement]](DRETURN)
      case SBooleanType =>
        List[CodeElement[InstructionElement]](IRETURN)
      case SInductiveType(_) =>
        List[CodeElement[InstructionElement]](ARETURN)
      case SStringType =>
        List[CodeElement[InstructionElement]](ARETURN)
    }
    METHOD(PUBLIC STATIC, function.name, function.getJavaMethodDescriptor, CODE(code ++ returnCode: _*).MAXLOCALS(data.nextVariableIndex()))
  }

  override def visitParameter(parameter: SParameter, data: BytecodeVisitorData): Any = {
    data.variableMap += (parameter.name -> data.nextVariableIndex(parameter.parameterType.stackSize))
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
      //TODO: unit is not done yet
      List(PUTFIELD(className, fieldName, "L/java/lang/Void;"))
    case SFloatType =>
      List(PUTFIELD(className, fieldName, "D"))
    case SNoTypeYet =>
      throw new RuntimeException("undexpected type " + varType + " while trying to store variable")
  }

  override def visitDataCase(dataCase: SDataCase, data: BytecodeVisitorData): CLASS[(Map[PC, InstructionElement], List[String])] = {
    val fields = dataCase.argTypes.zipWithIndex.foldLeft(List.empty[FIELD]) {
      case (l, (t, n)) =>
        FIELD(
          accessModifiers = PUBLIC,
          name = "field" + n,
          descriptor = t.fieldDescriptor
        ) :: l
    }
    val superCode = List[CodeElement[InstructionElement]](
      ALOAD_0,
      INVOKESPECIAL("java/lang/Object", isInterface = false, "<init>", "()V")
    )
    val assignmentCode = dataCase.argTypes.zipWithIndex.foldLeft((1, List.empty[CodeElement[InstructionElement]])) {
      case ((stackIndex, l), (t, n)) =>
        val loadInstructions = List[InstructionElement](ALOAD_0, getLoadInstruction(t, stackIndex))
        (stackIndex + t.stackSize, l ++ loadInstructions ++ storeObjectVariable(t, dataCase.name, "field" + n))
    }._2
    val descriptorParams = dataCase.argTypes.foldLeft("") {
      case (s, t) => s + t.fieldDescriptor
    }
    val constructor = METHOD(
      accessModifiers = PUBLIC,
      name = "<init>",
      descriptor = s"($descriptorParams)V",
      CODE(superCode ++ assignmentCode :+ CodeElement.instructionToInstructionElement(RETURN): _*)
    )

    val appendFieldCode = dataCase.argTypes.zipWithIndex.foldLeft(List.empty[CodeElement[InstructionElement]]) {
      case (l, (t, n)) =>
        val fieldDescriptor = if (t.isInstanceOf[SInductiveType]) "Ljava/lang/Object;" else t.fieldDescriptor
        val commaCode = if (n >= dataCase.argTypes.size - 1)
          List.empty[CodeElement[InstructionElement]]
        else
          List[CodeElement[InstructionElement]](
            LoadString(", "),
            INVOKEVIRTUAL("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
          )
        l ++ List[CodeElement[InstructionElement]](
          ALOAD_0,
          GETFIELD(dataCase.name, "field" + n, t.fieldDescriptor),
          INVOKEVIRTUAL("java/lang/StringBuilder", "append", s"($fieldDescriptor)Ljava/lang/StringBuilder;")
        ) ++ commaCode
    }

    val parenStartCode = if (dataCase.argTypes.isEmpty)
      List[CodeElement[InstructionElement]]()
    else
      List[CodeElement[InstructionElement]](
        LoadString("("),
        INVOKEVIRTUAL("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
      )

    val parenEndCode = if (dataCase.argTypes.isEmpty)
      List[CodeElement[InstructionElement]]()
    else
      List[CodeElement[InstructionElement]](
        LoadString(")"),
        INVOKEVIRTUAL("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
      )

    val toStringCode = List[CodeElement[InstructionElement]](
      NEW("java/lang/StringBuilder"),
      DUP,
      LoadString(dataCase.name),
      INVOKESPECIAL("java/lang/StringBuilder", isInterface = false, "<init>", "(Ljava/lang/String;)V")
    ) ++ parenStartCode ++ appendFieldCode ++ parenEndCode ++ List[CodeElement[InstructionElement]](
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
    val data = BytecodeVisitorData(program, "Program", Map.empty)
    visitProgram(program, data)
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
    programClass.toDA()._1 :: program.dataDeclarations.flatMap(d => visitData(d, data))
  }
}
