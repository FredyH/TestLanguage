package de.pylamo.trees

import de.pylamo.language.SType

case class SData(name: String, typeParams: List[String], cases: List[SDataCase]) extends STree {

}

case class SDataCase(name: String, argTypes: List[SType], dataName: String = "") extends STree
