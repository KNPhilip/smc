package smc.generators

object CodeGeneratorFactory {
  def create(language: String, strategy: String, outputPath: String): CodeGenerator = {
    if (strategy.toLowerCase == "nsc") {
      new NscCodeGenerator(language, outputPath)
    } else {
      throw new IllegalArgumentException(s"Unknown strategy \"$strategy\".")
    }
  }
}
