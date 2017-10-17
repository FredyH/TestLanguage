package de.pylamo.trees

import de.pylamo.language._

/**
  * Created by Fredy on 11.10.2017.
  */
case class ParameterList(parameters: List[SParameter]) extends STree

case class SFunction(name: String, returnType: SType, parameters: ParameterList, body: StatementList) extends STree {

  private def getJavaTypeName(stype: SType): String = stype match {
    case SBooleanType => "Z"
    case SIntType => "J"
    case SFloatType => "D"
    case SStringType => "Ljava/lang/String;"
    case SUnitType => "V"
    case SInductiveType(dataName) => s"L$dataName;"
    case _ => throw new RuntimeException("Wrong type")
  }

  def getJavaMethodDescriptor: String = {
    val params = parameters.parameters.foldLeft("") {
      case (str, param) =>
        str + getJavaTypeName(param.parameterType)
    }
    "(" + params + ")" + getJavaTypeName(returnType)
  }

}
