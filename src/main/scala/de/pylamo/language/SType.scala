package de.pylamo.language

import org.apache.bcel.generic.{ObjectType, Type}

/**
  * Created by Fredy on 11.10.2017.
  */

sealed trait SType {
  def fieldDescriptor: String

  def stackSize = 1

  def getBCELType: Type = this match {
    case SBooleanType => Type.BOOLEAN
    case SIntType => Type.LONG
    case SFloatType => Type.DOUBLE
    case SStringType => Type.STRING
    case SUnitType => Type.VOID
    case SInductiveType(name) => new ObjectType(name)
    case _ => throw new RuntimeException("Wrong type")
  }
}

sealed trait SPrimitiveType extends SType

case class SInductiveType(name: String) extends SType {
  override def fieldDescriptor = s"L$name;"
}

case object SBooleanType extends SPrimitiveType {
  override def fieldDescriptor = "Z"
}

sealed trait SNumericType extends SPrimitiveType

case object SIntType extends SNumericType {
  override def fieldDescriptor = "J"

  override def stackSize = 2
}

case object SFloatType extends SNumericType {
  override def fieldDescriptor = "D"

  override def stackSize = 2
}

case object SStringType extends SPrimitiveType {
  override def fieldDescriptor = "Ljava/lang/String;"
}

case object SUnitType extends SPrimitiveType {
  override def fieldDescriptor = "Ljava/lang/Void;"
}

case object SNoTypeYet extends SType {
  override def fieldDescriptor =
    throw new RuntimeException("NoTypeYet does not have field descriptor")
}