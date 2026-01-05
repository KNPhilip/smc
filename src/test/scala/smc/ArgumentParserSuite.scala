package smc

import munit.FunSuite

final class ArgumentParserSuite extends FunSuite {
  test("No arguments are not allowed") {
    intercept[ArgumentParserException] {
      val parser = new ArgumentParser(List.empty)
      parser.run()
    }
  }

  test("Missing input path arg is not allowed") {
    intercept[ArgumentParserException] {
      val parser = new ArgumentParser(List("-xyz"))
      parser.run()
    }
  }

  test("Missing input path value is not allowed") {
    intercept[ArgumentParserException] {
      val parser = new ArgumentParser(List("-i"))
      parser.run()
    }
  }

  test("Missing output path arg is not allowed") {
    intercept[ArgumentParserException] {
      val parser = new ArgumentParser(List("-i", "My/Path"))
      parser.run()
    }
  }

  test("Missing output path value is not allowed") {
    intercept[ArgumentParserException] {
      val parser = new ArgumentParser(List("-i", "My/Path", "-o"))
      parser.run()
    }
  }

  test("Unsupported languages are not allowed") {
    intercept[ArgumentParserException] {
      val parser = new ArgumentParser(List("-i", "P", "-o", "P", "-l", "MyUnsupportedLanguage"))
      parser.run()
    }
  }

  test("Unsupported strategies are not allowed") {
    intercept[ArgumentParserException] {
      val parser = new ArgumentParser(List("-i", "P", "-o", "P", "-l", "Scala", "-s", "MyUnsupportedStrategy"))
      parser.run()
    }
  }

  test("Default language is Scala") {
    val parser = new ArgumentParser(List("-i", "My/Path", "-o", "My/Path"))
    parser.run()
    assertEquals(parser.language, "Scala")
  }

  test("Default strategy is Selection") {
    val parser = new ArgumentParser(List("-i", "My/Path", "-o", "My/Path"))
    parser.run()
    assertEquals(parser.strategy, "Selection")
  }

  test("Correctly parses input path arg") {
    val parser = new ArgumentParser(List("-i", "Users/dkphkrni/Source.txt", "-o", "My/Path"))
    parser.run()
    assertEquals(parser.inputPath, "Users/dkphkrni/Source.txt")
  }

  test("If no input file extension is given, assume it is .sm") {
    val parser = new ArgumentParser(List("-i", "Users/dkphkrni/Source", "-o", "My/Path"))
    parser.run()
    assertEquals(parser.inputPath, "Users/dkphkrni/Source.sm")
  }

  test("Correctly parses output path arg") {
    val parser = new ArgumentParser(List("-i", "My/Path", "-o", "Users/dkphkrni/Source"))
    parser.run()
    assertEquals(parser.outputPath, "Users/dkphkrni/Source")
  }

  test("Correctly parses language arg") {
    val parser = new ArgumentParser(List("-i", "P", "-o", "P", "-l", "Scala"))
    parser.run()
    assertEquals(parser.language, "Scala")
  }

  test("Correctly parses language arg with wrong casing") {
    val parser = new ArgumentParser(List("-i", "P", "-o", "P", "-l", "sCaLA"))
    parser.run()
    assertEquals(parser.language, "Scala")
  }

  test("Correctly parses strategy arg") {
    val parser = new ArgumentParser(List("-i", "P", "-o", "P", "-s", "Selection"))
    parser.run()
    assertEquals(parser.strategy, "Selection")
  }

  test("Correctly parses strategy arg with wrong casing") {
    val parser = new ArgumentParser(List("-i", "P", "-o", "P", "-s", "sElEcTiOn"))
    parser.run()
    assertEquals(parser.strategy, "Selection")
  }

  test("Argument order doesn't matter") {
    val parser = new ArgumentParser(List(
      "-l", "SCALA",
      "-o", "My/Output/Path",
      "-s", "sElEcTiOn",
      "-i", "My/Input/Path"))

    parser.run()

    assertEquals(parser.language, "Scala")
    assertEquals(parser.strategy, "Selection")
    assertEquals(parser.inputPath, "My/Input/Path.sm")
    assertEquals(parser.outputPath, "My/Output/Path")
  }
}
