package de.pylamo.trees

import de.pylamo.language._
import de.pylamo.visitors.irgeneration.BytecodeVisitorData
import org.apache.bcel.Const
import org.apache.bcel.generic._

/**
  * Created by Fredy on 11.10.2017.
  */
sealed abstract class SExpression(_expressionType: SType = SNoTypeYet) extends STree {
  val factory: InstructionFactory = null

  def exprType: SType = _expressionType

  def appendBytecodeInstructions(data: BytecodeVisitorData): Unit
}

sealed trait MatchPattern extends STree

case object Wildcard extends MatchPattern

case class ConstructorPattern(name: String, subPatterns: List[MatchPattern]) extends MatchPattern


sealed trait ConstantExpression extends SExpression with MatchPattern

//region Constants/Variables

case class BooleanConstant(value: Boolean) extends ConstantExpression {

  override def exprType: SType = SBooleanType

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = {
    data.instructionList.append(data.instructionFactory.createConstant(if (value) 1 else 0))
  }
}

case class FloatConstant(value: Double) extends ConstantExpression {

  override def exprType: SType = SFloatType

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = {
    data.instructionList.append(data.instructionFactory.createConstant(value))
  }
}

case class IntConstant(value: Long) extends ConstantExpression {

  override def exprType: SType = SIntType

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit =
    data.instructionList.append(data.instructionFactory.createConstant(value))
}

case class StringConstant(value: String) extends ConstantExpression {

  override def exprType: SType = SStringType

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit =
    data.instructionList.append(data.instructionFactory.createConstant(value))
}

case class SVariable(name: String, override val exprType: SType = SNoTypeYet) extends SExpression with MatchPattern {
  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit =
    exprType match {
      case SBooleanType =>
        data.instructionList.append(new ILOAD(data.getVariable(name)))
      case SIntType =>
        data.instructionList.append(new LLOAD(data.getVariable(name)))
      case SStringType =>
        data.instructionList.append(new ALOAD(data.getVariable(name)))
      case SFloatType =>
        data.instructionList.append(new DLOAD(data.getVariable(name)))
      case SInductiveType(_) =>
        data.instructionList.append(new ALOAD(data.getVariable(name)))
    }
}

//endregion

case class ArgumentList(arguments: List[SExpression]) extends STree

trait FunctionLike extends SExpression {


  def getBCELType(sType: SType): Type = sType match {
    case SBooleanType => Type.BOOLEAN
    case SIntType => Type.LONG
    case SFloatType => Type.DOUBLE
    case SStringType => Type.STRING
    case SUnitType => Type.VOID
    case SInductiveType(name) => new ObjectType(name)
    case _ => throw new RuntimeException("Wrong type")
  }

}

case class DataConstructor(name: String, arguments: ArgumentList) extends FunctionLike {


  override def exprType = SInductiveType(name)

  def getBCELArgumentTypes(program: SProgram): Array[Type] = {
    val constructor = getDataCase(program).get
    constructor.argTypes.map {
      t => getBCELType(t)
    }.toArray
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


  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = {}
}

case class FunctionCallReference(name: String, arguments: ArgumentList,
                                 override val exprType: SType) extends FunctionLike {
  def getFunction(program: SProgram): Option[SFunction] = {
    program.functions.find(_.name == name)
  }

  def getBCELReturnType: Type = getBCELType(exprType)

  def getBCELArgumentTypes(program: SProgram): Array[Type] = {
    val function = getFunction(program).get
    function.parameters.parameters.map {
      p => getBCELType(p.parameterType)
    }.toArray
  }

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit =
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

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = exprType match {
    case SBooleanType =>
      data.instructionList.append(new ICONST(1))
      data.instructionList.append(new IXOR())
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

case class UnaryMinus(subExpression: SExpression, override val exprType: SType = SNoTypeYet) extends UnaryOperation {
  def newInstance(arg: SExpression, exprType: SType = exprType): UnaryMinus = UnaryMinus(arg, exprType)

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = exprType match {
    case SIntType => data.instructionList.append(new INEG())
    case SFloatType => data.instructionList.append(new DNEG())
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

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit =
    subExpression.exprType match {
      case SIntType =>
      case SFloatType => data.instructionList.append(new D2L())
      case _ => throw new RuntimeException("Wrong type of expression")
    }

  override val exprType: SType = SIntType
}

case class FloatCast(subExpression: SExpression) extends CastOperation {
  override def newInstance(subExpression: SExpression, exprType: SType = SFloatType): UnaryOperation = {
    FloatCast(subExpression)
  }

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = subExpression.exprType match {
    case SIntType => data.instructionList.append(new L2D())
    case SFloatType =>
    case _ => throw new RuntimeException("Wrong type of expression")
  }

  override val exprType: SType = SFloatType
}

case class StringCast(subExpression: SExpression) extends CastOperation {
  override def newInstance(subExpression: SExpression, exprType: SType = SStringType): UnaryOperation = {
    StringCast(subExpression)
  }

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = subExpression.exprType match {
    case SStringType =>
    case SFloatType =>
      data.instructionList.append(data.instructionFactory.createInvoke("java.lang.Double", "valueOf", new ObjectType("java.lang.Double"), Array(Type.DOUBLE), Const.INVOKESTATIC))
      data.instructionList.append(data.instructionFactory.createInvoke("java.lang.Object", "toString", Type.STRING, Array(), Const.INVOKEVIRTUAL))
    case SIntType =>
      data.instructionList.append(data.instructionFactory.createInvoke("java.lang.Long", "valueOf", new ObjectType("java.lang.Long"), Array(Type.LONG), Const.INVOKESTATIC))
      data.instructionList.append(data.instructionFactory.createInvoke("java.lang.Object", "toString", Type.STRING, Array(), Const.INVOKEVIRTUAL))
    case SBooleanType =>
      data.instructionList.append(data.instructionFactory.createInvoke("java.lang.Boolean", "valueOf", new ObjectType("java.lang.Boolean"), Array(Type.BOOLEAN), Const.INVOKESTATIC))
      data.instructionList.append(data.instructionFactory.createInvoke("java.lang.Object", "toString", Type.STRING, Array(), Const.INVOKEVIRTUAL))
    case SInductiveType(_) =>
      data.instructionList.append(data.instructionFactory.createInvoke("java.lang.Object", "toString", Type.STRING, Array(), Const.INVOKEVIRTUAL))
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

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = {
    data.instructionList.append(new SWAP)
    data.instructionList.append(data.instructionFactory.createNew("java.lang.StringBuilder"))
    data.instructionList.append(new DUP_X2())
    data.instructionList.append(data.instructionFactory.createInvoke("java.lang.StringBuilder", "<init>", Type.VOID, Array(Type.STRING), Const.INVOKESPECIAL))
    data.instructionList.append(data.instructionFactory.createInvoke("java.lang.StringBuilder", "append", new ObjectType("java.lang.StringBuilder"), Array(Type.STRING), Const.INVOKEVIRTUAL))
    data.instructionList.append(data.instructionFactory.createInvoke("java.lang.Object", "toString", Type.STRING, Array(), Const.INVOKEVIRTUAL))
  }

  override def exprType: SType = SStringType
}

sealed trait NumericBinaryOperation extends BinaryOperation

case class Addition(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends NumericBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    Addition(left, right, exprType)

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = exprType match {
    case SIntType => data.instructionList.append(new LADD())
    case SFloatType => data.instructionList.append(new DADD())
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

case class Subtraction(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends NumericBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    Subtraction(left, right, exprType)

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = exprType match {
    case SIntType => data.instructionList.append(new LSUB())
    case SFloatType => data.instructionList.append(new DSUB())
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

case class Multiplication(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends NumericBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    Multiplication(left, right, exprType)

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = exprType match {
    case SIntType => data.instructionList.append(new LMUL())
    case SFloatType => data.instructionList.append(new DMUL())
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

case class Division(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends NumericBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    Division(left, right, exprType)

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = exprType match {
    case SIntType => data.instructionList.append(new LDIV())
    case SFloatType => data.instructionList.append(new DDIV())
    case _ => throw new RuntimeException("Wrong type of expression")
  }
}

//endregion

//region Comparison Operators

sealed trait ComparisonOperation extends BinaryOperation {
  override def exprType: SType = SBooleanType

  protected def createIfInstruction: IfInstruction

  def appendComparisonInstructions(data: BytecodeVisitorData): Unit = {
    left.exprType match {
      case SIntType =>
        data.instructionList.append(new LCMP)
      case SFloatType =>
        data.instructionList.append(new DCMPG())
      case _ => throw new RuntimeException("Wrong type of expression")
    }
  }

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit = {
    val ifInstruction = createIfInstruction
    data.instructionList.append(ifInstruction)
    data.instructionList.append(new ICONST(0))
    val goto = new GOTO(null)
    data.instructionList.append(goto)
    ifInstruction.setTarget(data.instructionList.append(new ICONST(1)))
    goto.setTarget(data.instructionList.append(new NOP))
  }
}

case class GreaterEqual(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    GreaterEqual(left, right)


  override protected def createIfInstruction = new IFGE(null)

}

case class Greater(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    Greater(left, right)

  override protected def createIfInstruction = new IFGT(null)
}

case class LessEquals(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    LessEquals(left, right)

  override protected def createIfInstruction = new IFLE(null)
}

case class Less(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    Less(left, right)

  override protected def createIfInstruction = new IFLT(null)
}

case class Equals(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    Equals(left, right)


  override def appendComparisonInstructions(data: BytecodeVisitorData): Unit = Equals.appendCmpInstruction(exprType, data)

  override protected def createIfInstruction = new IFEQ(null)
}

object Equals {
  def appendCmpInstruction(cmpType: SType, data: BytecodeVisitorData): Unit = {
    val index = data.nextLabelIndex()
    val ifInstruction = cmpType match {
      case SBooleanType =>
        new IF_ICMPEQ(null)
      case SIntType =>
        data.instructionList.append(new LCMP())
        new IFEQ(null)
      case SFloatType =>
        data.instructionList.append(new DCMPG())
        new IFEQ(null)
      case _ => throw new RuntimeException("Wrong type of expression")
    }
    data.instructionList.append(ifInstruction)
    data.instructionList.append(new ICONST(0))
    val goto = new GOTO(null)
    data.instructionList.append(goto)
    ifInstruction.setTarget(data.instructionList.append(new ICONST(1)))
    goto.setTarget(data.instructionList.append(new NOP))
  }
}

case class NotEquals(left: SExpression, right: SExpression) extends ComparisonOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = SBooleanType): BinaryOperation =
    NotEquals(left, right)

  override def appendComparisonInstructions(data: BytecodeVisitorData): Unit = Equals.appendCmpInstruction(exprType, data)

  override protected def createIfInstruction = new IFNE(null)
}

//endregion


//region Boolean Binary Operations


sealed trait BooleanBinaryOperation extends BinaryOperation {
  override val exprType: SType = SNoTypeYet
}

case class BAnd(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends BooleanBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    BAnd(left, right, exprType)

  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit =
    data.instructionList.append(new IAND())
}

case class BOr(left: SExpression, right: SExpression, override val exprType: SType = SNoTypeYet) extends BooleanBinaryOperation {
  override def newInstance(left: SExpression, right: SExpression, exprType: SType = exprType): BinaryOperation =
    BOr(left, right, exprType)


  override def appendBytecodeInstructions(data: BytecodeVisitorData): Unit =
    data.instructionList.append(new IOR())
}

//endregion