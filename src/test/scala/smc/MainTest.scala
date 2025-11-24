package smc

class MainTest extends munit.FunSuite {
  test("Addition works") {
    val calculator = new Calculator()
    assertEquals(calculator.plus(2, 2), 4)
  }
}