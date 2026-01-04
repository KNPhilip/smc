package smc.generators

import munit.FunSuite

final class CodeGeneratorFactorySuite extends FunSuite {
  test("Factory can create code generator for selection strategy") {
    val generator: CodeGenerator = CodeGeneratorFactory.create("", "selection", "")
    val correctType: Boolean = generator.isInstanceOf[SelectionCodeGenerator]
    assert(correctType)
  }

  test("Factory can create code generator for selection strategy ignoring casing") {
    val generator: CodeGenerator = CodeGeneratorFactory.create("", "sElEcTIoN", "")
    val correctType: Boolean = generator.isInstanceOf[SelectionCodeGenerator]
    assert(correctType)
  }

  test("Factory throws exception for unknown strategies") {
    intercept[IllegalArgumentException] {
      CodeGeneratorFactory.create("", "gibberish strategy", "")
    }
  }
}
