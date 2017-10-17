package de.pylamo.trees

import de.pylamo.language.SType

/**
  * Created by Fredy on 13.10.2017.
  */
case class SProgram(functions: List[SFunction], dataDeclarations: List[SData]) extends STree {

}
