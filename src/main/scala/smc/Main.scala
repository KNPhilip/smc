package smc

import smc.lexicalAnalyzer.LexicalAnalyzer
import smc.syntaxAnalyzer.SyntacticalAnalyzer
import smc.semanticAnalyzer.SemanticAnalyzer
import smc.optimizer.Optimizer
import smc.generators.CodeGeneratorFactory

import java.io.IOException
import java.nio.file.{Files, Paths}

object Main {
    private var language: String = "Scala"
    private var strategy: String = "Selection"
    private var inputPath: String = ""
    private var outputPath: String = ""

    @main
    def entrypoint(args: String*): Unit = {
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
        val argParser = new ArgumentParser(args)
        argParser.run()

        language = argParser.language
        strategy = argParser.strategy
        inputPath = argParser.inputPath
        outputPath = argParser.outputPath
    }

    private def compile(sourceCode: String): Unit = {
        val parser = new SyntacticalAnalyzer()
        LexicalAnalyzer(parser).lex(sourceCode)

        val machine = parser.getStateMachineSyntax
        val errors = new SemanticAnalyzer().analyze(machine).toList

        if (errors.isEmpty) {
            val optimizedMachine = new Optimizer().optimize(machine.machines.head)
            val generator = CodeGeneratorFactory.create(language, strategy, outputPath)
            generator.generate(optimizedMachine)
        }
    }
}
