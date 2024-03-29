package de.pylamo

import java.io.{File, FileOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream}

import de.pylamo.visitors.irgeneration.BytecodeVisitor
import de.pylamo.visitors.semantics.{SemanticsVisitor, TypeVisitor}
import org.apache.bcel.verifier.GraphicalVerifier

import scala.io.Source
import scala.sys.process._

/**
  * Created by Fredy on 11.10.2017.
  */
object Main {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("Test.tl").getLines().mkString("\n")
    val parsed = Parser.parseProgram(source)
    val checkedProgram = SemanticsVisitor.visitProgram(parsed)
    val typedProgram = TypeVisitor.visitProgram(checkedProgram)
    val classFiles = BytecodeVisitor.visitProgram(typedProgram).map(_.getJavaClass)
    val outputFile = new File("output.jar")
    outputFile.delete()
    val jarOutputStream = new ZipOutputStream(new FileOutputStream(outputFile))
    new File("output").mkdir()
    new File("output").listFiles().foreach(_.delete())
    classFiles.foreach {
      classFile =>
        //Writes class file into jar
        val fileName = classFile.getClassName + ".class"
        val assembledClass = classFile.getBytes
        jarOutputStream.putNextEntry(new ZipEntry(fileName))
        jarOutputStream.write(assembledClass)
        val classFOS = new FileOutputStream("output/" + fileName)
        classFOS.write(assembledClass)
        classFOS.close()
    }
    jarOutputStream.putNextEntry(new ZipEntry("META-INF/MANIFEST.MF"))
    jarOutputStream.write(
      """|Manifest-Version: 1.0
         |Main-Class: Program
         |""".stripMargin.getBytes)
    jarOutputStream.close()
    println()
    println("---------------------------------------")
    println("Running program")
    println("---------------------------------------")
    println()
    """java -jar output.jar""".!<
  }
}