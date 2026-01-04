package smc.generators

object CodeGeneratorFactory {
  def create(language: String, strategy: String, outputPath: String): CodeGenerator = {
    if (strategy.toLowerCase == "selection") {
      new SelectionCodeGenerator(language, outputPath)
    } else {
      throw new IllegalArgumentException(s"Unknown strategy \"$strategy\".")
    }
  }
}
