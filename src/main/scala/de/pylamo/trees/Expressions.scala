package de.pylamo.trees

import de.pylamo.language._
import de.pylamo.visitors.irgeneration.BytecodeVisitorData
import org.opalj.ba.{CODE, CodeElement, InstructionElement}
import org.opalj.br.instructions._

/**
  * Created by Fredy on 11.10.2017.
  */
sealed abstract class SExpression(_expressionType: SType = SNoTypeYet) extends STree {
  def exprType: SType = _expressionType

  def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]]
}

//region Constants/Variables

case class BooleanConstant(value: Boolean) extends SExpression(SBooleanType) {
  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] =
    if (value)
      List(ICONST_1)
    else
      List(ICONST_0)
}

case class FloatConstant(value: Double) extends SExpression(SFloatType) {
  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] =
    List(LoadDouble(value))
}

case class IntConstant(value: Long) extends SExpression(SIntType) {
  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] =
    List(LoadLong(value))
}

case class StringConstant(value: String) extends SExpression(SStringType) {
  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] =
    List(LoadString(value))
}

case class SVariable(name: String, override val exprType: SType = SNoTypeYet) extends SExpression {
  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] =
    exprType match {
      case SBooleanType => List(ILOAD(data.variableMap(name)))
      case SIntType => List(LLOAD(data.variableMap(name)))
      case SStringType => List(ALOAD(data.variableMap(name)))
      case SFloatType => List(DLOAD(data.variableMap(name)))
      case SInductiveType(_) => List(ALOAD(data.variableMap(name)))
    }
}

//endregion

case class ArgumentList(arguments: List[SExpression]) extends STree

trait FunctionLike extends SExpression {


  def getJavaTypeName(stype: SType): String = stype match {
    case SBooleanType => "Z"
    case SIntType => "J"
    case SFloatType => "D"
    case SStringType => "Ljava/lang/String;"
    case SUnitType => "V"
    case SInductiveType(name) => s"L$name;"
    case _ => throw new RuntimeException("Wrong type")
  }

}

case class DataConstructor(name: String, arguments: ArgumentList) extends FunctionLike {


  override def exprType = SInductiveType(name)


  def getJavaMethodDescriptor(program: SProgram): String = {
    val constructor = getDataCase(program).get
    val params = constructor.argTypes.foldLeft("") {
      case (str, t) =>
        str + getJavaTypeName(t)
    }
    "(" + params + ")V"
  }

  //TODO: Improve massively
  def getData(program: SProgram): Option[SData] = {
    (for (data <- program.dataDeclarations;
          dataCase <- data.cases if dataCase.name == name)
      yield data).headOption
  }

  def getDataCase(program: SProgram): Option[SDataCase] = {
    (for (data <- program.dataDeclarations;
          dataCase <- data.cases if dataCase.name == name)
      yield dataCase).headOption
  }

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[InstructionElement] = {
    List()
  }
}

case class FunctionCallReference(name: String, arguments: ArgumentList,
                                 override val exprType: SType) extends FunctionLike {
  def getFunction(program: SProgram): Option[SFunction] = {
    program.functions.find(_.name == name)
  }

  def getJavaMethodDescriptor(program: SProgram): String = {
    val function = getFunction(program).get
    val params = function.parameters.parameters.foldLeft("") {
      case (str, t) =>
        str + getJavaTypeName(t.parameterType)
    }
    "(" + params + ")" + getJavaTypeName(exprType)
  }

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] =
    throw new RuntimeException("Not implemented")
}

sealed trait Operation extends SExpression {
}

sealed trait UnaryOperation extends Operation {
  val subExpression: SExpression

  def newInstance(subExpression: SExpression, exprType: SType = exprType): UnaryOperation
}

//region Unary Operations

object UnaryOperation {
  def unapply(arg: UnaryOperation): Option[SExpression] = Some(arg.subExpression)
}

case class BNegation(subExpression: SExpression, override val exprType: SType = SNoTypeYet) extends UnaryOperation {
  def newInstance(arg: SExpression, exprType: SType = exprType): BNegation = BNegation(arg, exprType)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = exprType match {
    case SBooleanType => List(Symbol("Hello"), ICONST_1, IXOR)
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

case class UnaryMinus(subExpression: SExpression, override val exprType: SType = SNoTypeYet) extends UnaryOperation {
  def newInstance(arg: SExpression, exprType: SType = exprType): UnaryMinus = UnaryMinus(arg, exprType)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = exprType match {
    case SIntType => List(INEG)
    case SFloatType => List(DNEG)
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

//endregion

//region Casts

sealed trait CastOperation extends UnaryOperation {

}

case class IntCast(subExpression: SExpression) extends CastOperation {
  override def newInstance(subExpression: SExpression, exprType: SType = SIntType): UnaryOperation = {
    FloatCast(subExpression)
  }

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] =
    subExpression.exprType match {
      case SIntType => List()
      case SFloatType => List(D2L)
      case _ => throw new RuntimeException("Wrong type of expression")
    }

  override val exprType: SType = SIntType
}

case class FloatCast(subExpression: SExpression) extends CastOperation {
  override def newInstance(subExpression: SExpression, exprType: SType = SFloatType): UnaryOperation = {
    FloatCast(subExpression)
  }

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = subExpression.exprType match {
    case SIntType => List(L2D)
    case SFloatType => List()
    case _ => throw new RuntimeException("Wrong type of expression")
  }

  override val exprType: SType = SFloatType
}

case class StringCast(subExpression: SExpression) extends CastOperation {
  override def newInstance(subExpression: SExpression, exprType: SType = SStringType): UnaryOperation = {
    StringCast(subExpression)
  }

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = subExpression.exprType match {
    case SStringType => Nil
    case SFloatType => List(
      INVOKESTATIC("java/lang/Double", isInterface = false, "valueOf", "(D)Ljava/lang/Double;"),
      INVOKEVIRTUAL("java/lang/Object", "toString", "()Ljava/lang/String;")
    )
    case SIntType => List(
      INVOKESTATIC("java/lang/Long", isInterface = false, "valueOf", "(J)Ljava/lang/Long;"),
      INVOKEVIRTUAL("java/lang/Object", "toString", "()Ljava/lang/String;")
    )
    case SBooleanType => List(
      INVOKESTATIC("java/lang/Boolean", isInterface = false, "valueOf", "(Z)Ljava/lang/Boolean;"),
      INVOKEVIRTUAL("java/lang/Object", "toString", "()Ljava/lang/String;")
    )
    case SInductiveType(_) => List(
      INVOKEVIRTUAL("java/lang/Object", "toString", "()Ljava/lang/String;")
    )
    case t => throw new RuntimeException(s"Invalid type to be casted to string: $t")
  }

  override val exprType: SType = SFloatType
}

//endregion


sealed trait BinaryOperation extends Operation {
  val left: SExpression
  val right: SExpression

  def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation

}

object BinaryOperation {
  def unapply(arg: BinaryOperation): Option[(SExpression, SExpression)] = Some(arg.left, arg.right)
}

//region Numeric Binary Operations

case class StringConcatenation(left: SExpression, right: SExpression) extends BinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType): StringConcatenation =
    StringConcatenation(left, right)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    List(
      SWAP,
      NEW("java/lang/StringBuilder"),
      DUP_X2,
      SWAP,
      INVOKESPECIAL("java/lang/StringBuilder", isInterface = false, "<init>", "(Ljava/lang/String;)V"),
      INVOKEVIRTUAL("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;"),
      INVOKEVIRTUAL("java/lang/Object", "toString", "()Ljava/lang/String;")
    )
  }

  override def exprType: SType = SStringType
}

sealed trait NumericBinaryOperation extends BinaryOperation

case class Addition(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends NumericBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    Addition(left, right, exprType)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = exprType match {
    case SIntType => List(LADD)
    case SFloatType => List(DADD)
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

case class Subtraction(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends NumericBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    Subtraction(left, right, exprType)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = exprType match {
    case SIntType => List(LSUB)
    case SFloatType => List(DSUB)
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

case class Multiplication(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends NumericBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    Multiplication(left, right, exprType)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = exprType match {
    case SIntType => List(LMUL)
    case SFloatType => List(DMUL)
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

case class Division(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends NumericBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    Division(left, right, exprType)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = exprType match {
    case SIntType => List(LDIV)
    case SFloatType => List(DDIV)
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

//endregion

//region Comparison Operators

sealed trait ComparisonOperation extends BinaryOperation {
  override def exprType: SType = SBooleanType
}

case class GreaterEqual(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    GreaterEqual(left, right)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    val index = data.nextLabelIndex()
    val trueLabel = Symbol("cmpTrue" + index)
    val falseLabel = Symbol("cmpFalse" + index)
    left.exprType match {
      case SIntType => List(LCMP, IFGE(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case SFloatType => List(DCMPG, IFGE(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case _ => throw new RuntimeException("Wrong type of expression")
    }
  }
}

case class Greater(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    Greater(left, right)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    val index = data.nextLabelIndex()
    val trueLabel = Symbol("cmpTrue" + index)
    val falseLabel = Symbol("cmpFalse" + index)
    left.exprType match {
      case SIntType => List(LCMP, IFGT(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case SFloatType => List(DCMPG, IFGT(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case _ => throw new RuntimeException("Wrong type of expression")
    }
  }
}

case class LessEquals(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    LessEquals(left, right)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    val index = data.nextLabelIndex()
    val trueLabel = Symbol("cmpTrue" + index)
    val falseLabel = Symbol("cmpFalse" + index)
    left.exprType match {
      case SIntType => List(LCMP, IFLE(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case SFloatType => List(DCMPG, IFLE(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case _ => throw new RuntimeException("Wrong type of expression")
    }
  }
}

case class Less(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    Less(left, right)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    val index = data.nextLabelIndex()
    val trueLabel = Symbol("cmpTrue" + index)
    val falseLabel = Symbol("cmpFalse" + index)
    left.exprType match {
      case SIntType => List(LCMP, IFLT(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case SFloatType => List(DCMPG, IFLT(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case _ => throw new RuntimeException("Wrong type of expression")
    }
  }
}

case class Equals(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    Equals(left, right)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    val index = data.nextLabelIndex()
    val trueLabel = Symbol("cmpTrue" + index)
    val falseLabel = Symbol("cmpFalse" + index)
    left.exprType match {
      case SBooleanType => List(IF_ICMPEQ(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case SIntType => List(LCMP, IFEQ(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case SFloatType => List(DCMPG, IFEQ(trueLabel), ICONST_0, GOTO(falseLabel), trueLabel, ICONST_1, falseLabel)
      case _ => throw new RuntimeException("Wrong type of expression")
    }
  }
}

case class NotEquals(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    NotEquals(left, right)


  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = {
    val index = data.nextLabelIndex()
    val trueLabel = Symbol("cmpTrue" + index)
    val falseLabel = Symbol("cmpFalse" + index)
    left.exprType match {
      case SBooleanType => List(IF_ICMPEQ(trueLabel), ICONST_1, GOTO(falseLabel), trueLabel, ICONST_0, falseLabel)
      case SIntType => List(LCMP, IFEQ(trueLabel), ICONST_1, GOTO(falseLabel), trueLabel, ICONST_0, falseLabel)
      case SFloatType => List(DCMPG, IFEQ(trueLabel), ICONST_1, GOTO(falseLabel), trueLabel, ICONST_0, falseLabel)
      case _ => throw new RuntimeException("Wrong type of expression")
    }
  }
}

//endregion


//region Boolean Binary Operations


sealed trait BooleanBinaryOperation extends BinaryOperation {
  override val exprType: SType = SNoTypeYet
}

case class BAnd(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends BooleanBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    BAnd(left, right, exprType)

  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = List(IAND)
}

case class BOr(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends BooleanBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    BOr(left, right, exprType)


  override def getBytecodeInstructions(data: BytecodeVisitorData): List[CodeElement[InstructionElement]] = List(IOR)
}

//endregion