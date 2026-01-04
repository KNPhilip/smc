package smc

import java.io.IOException
import java.nio.file.{Files, Paths}
import smc.lexicalAnalyzer.LexicalAnalyzer
import smc.optimizer.Optimizer
import smc.semanticAnalyzer.SemanticAnalyzer
import smc.syntaxAnalyzer.SyntacticalAnalyzer

object Main {
    private var language: String = "Scala"
    private var strategy: String = "Nsc"
    private var inputPath: String = ""
    private var outputPath: String = ""

    @main
    def main(args: String*): Unit = {
        try {
            parseArguments(args.toList)
            val sourceCode = new String(Files.readAllBytes(Paths.get(inputPath)))
            compile(sourceCode)
        } catch {
            case e: IOException =>
                println(e.getMessage)
            case e: Exception =>
                println(s"Unexpected error: ${e.getMessage}")
        }
    }

    private def parseArguments(args: List[String]): Unit = {
        language = ""
        strategy = ""
        inputPath = ""
        outputPath = ""
    }

    private def compile(sourceCode: String): Unit = {
        val parser = new SyntacticalAnalyzer()
        LexicalAnalyzer(parser).lex(sourceCode)

        val machine = parser.getStateMachineSyntax
        val errors = new SemanticAnalyzer().analyze(machine).toList

        if (errors.isEmpty) {
            val optimizedMachine = new Optimizer().optimize(machine.machines.head)
            // Generate code here based on strategy, language, etc.
        }
    }
}
