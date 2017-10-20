package de.pylamo.trees

import de.pylamo.language.SType

/**
  * Created by Fredy on 13.10.2017.
  */
case class SProgram(functions: List[SFunction], dataDeclarations: List[SData]) extends STree {

  def findDataOrCase(name: String): Option[Either[SData, SDataCase]] = {
    (findData(name), findDataCase(name)) match {
      case (_, Some(c)) => Some(Right(c))
      case (Some(c), _) => Some(Left(c))
      case _ => None
    }
  }

  def findData(name: String): Option[SData] = {
    this.dataDeclarations.find(_.name == name)
  }

  def findDataCase(name: String): Option[SDataCase] = {
    this.dataDeclarations.flatMap(_.cases).find(_.name == name)
  }
}
