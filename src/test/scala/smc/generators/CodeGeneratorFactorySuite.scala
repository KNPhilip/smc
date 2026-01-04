package smc.generators

import munit.FunSuite

final class CodeGeneratorFactorySuite extends FunSuite {
  test("Factory can create code generator for NSC strategy") {
    val generator: CodeGenerator = CodeGeneratorFactory.create("", "nsc", "")
    val correctType: Boolean = generator.isInstanceOf[NscCodeGenerator]
    assert(correctType)
  }

  test("Factory can create code generator for NSC strategy ignoring casing") {
    val generator: CodeGenerator = CodeGeneratorFactory.create("", "nSC", "")
    val correctType: Boolean = generator.isInstanceOf[NscCodeGenerator]
    assert(correctType)
  }

  test("Factory throws exception for unknown strategies") {
    intercept[IllegalArgumentException] {
      CodeGeneratorFactory.create("", "gibberish strategy", "")
    }
  }
}
