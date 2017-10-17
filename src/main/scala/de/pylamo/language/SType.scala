package de.pylamo.language

/**
  * Created by Fredy on 11.10.2017.
  */
sealed trait SType {
  def fieldDescriptor: String

  def stackSize = 1
}

case class SInductiveType(name: String) extends SType {
  override def fieldDescriptor = s"L$name;"
}

case object SBooleanType extends SType {
  override def fieldDescriptor = "Z"
}

case object SIntType extends SType {
  override def fieldDescriptor = "J"

  override def stackSize = 2
}

case object SStringType extends SType {
  override def fieldDescriptor = "Ljava/lang/String;"
}

case object SUnitType extends SType {
  override def fieldDescriptor = "Ljava/lang/Void;"
}

case object SFloatType extends SType {
  override def fieldDescriptor = "D"

  override def stackSize = 2
}

case object SNoTypeYet extends SType {
  override def fieldDescriptor =
    throw new RuntimeException("NoTypeYet does not have field descriptor")
}