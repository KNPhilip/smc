package smc

import scala.annotation.tailrec

final class ArgumentParser(args: List[String]) {
  var language: String = "Scala"
  var strategy: String = "Selection"
  var inputPath: String = ""
  var outputPath: String = ""

  private val expectedArgsMessage =
    "Expected arguments are:\n" +
    "  -i \"Path-To-File\": Specify the path to the .sm file containing " +
    "your state machine definition. (Required)\n" +
    "  -o \"Output-Path\": Specify where to put the compiled files. (Required)\n" +
    "  -l \"Scala\": Specify output programming language. (Defaults to Scala)\n" +
    "  -s \"Selection\": Specify state machine strategy. (Defaults to Selection)"

  def run(): Unit = {
    if (args.isEmpty)
      throw new ArgumentParserException("\nWelcome to the State Machine Compiler.\n" +
        "In order to get started you need to pass in some arguments.\n\n" +
        s"$expectedArgsMessage\n\nFor more info check out https://github.com/KNPhilip/smc\n")

    parse(args)

    if (inputPath.isEmpty)
      throw new ArgumentParserException("Missing required argument: -i <input-path>")
    if (outputPath.isEmpty)
      throw new ArgumentParserException("Missing required argument: -o <output-path>")
  }

  @tailrec
  private def parse(remaining: List[String]): Unit = {
    val availableStrategies = List("Selection")
    val availableLanguages = List("Scala")

    remaining match {
      case Nil =>
        ()

      case "-i" :: value :: tail =>
        if (value.contains("."))
          inputPath = value
        else
          inputPath = s"$value.sm"

        parse(tail)

      case "-o" :: value :: tail =>
        outputPath = value
        parse(tail)

      case "-l" :: value :: tail =>
        val lang = value.toLowerCase.toPascalCase

        if (!availableLanguages.contains(lang))
          throw new ArgumentParserException(s"The language \"$lang\" is not currently supported. " +
            s"Supported languages are:\n${availableLanguages.mkString(", ")}")

        language = lang
        parse(tail)

      case "-s" :: value :: tail =>
        val strat = value.toLowerCase.toPascalCase

        if (!availableStrategies.contains(strat))
          throw new ArgumentParserException(s"The strategy \"$strat\" is invalid. " +
            s"Available strategies are:\n${availableStrategies.mkString(", ")}")

        strategy = strat
        parse(tail)

      case flag :: _ =>
        throw new ArgumentParserException(
          s"Unknown or malformed argument: $flag\n\n" + expectedArgsMessage)
    }
  }
}

final class ArgumentParserException(msg: String) extends Exception(msg)
