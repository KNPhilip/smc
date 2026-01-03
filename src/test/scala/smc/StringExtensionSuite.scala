package smc

import munit.FunSuite

final class StringExtensionSuite extends FunSuite {
  test("Camel case to camel case") {
    val expected = "myCase"
    val input = "myCase"
    assertEquals(input.toCamelCase, expected)
  }

  test("Pascal case to camel case") {
    val expected = "myCase"
    val input = "MyCase"
    assertEquals(input.toCamelCase, expected)
  }

  test("Snake case to camel case") {
    val expected = "myCase"
    val input = "my_case"
    assertEquals(input.toCamelCase, expected)
  }

  test("Kebab case to camel case") {
    val expected = "myCase"
    val input = "my-case"
    assertEquals(input.toCamelCase, expected)
  }

  test("Sentence to camel case") {
    val expected = "myCase"
    val input = "My Case"
    assertEquals(input.toCamelCase, expected)
  }

  test("Numbers in text to camel case") {
    val expected = "my2Case"
    val input = "My 2 Case"
    assertEquals(input.toCamelCase, expected)
  }

  test("Pascal case to pascal case") {
    val expected = "MyCase"
    val input = "MyCase"
    assertEquals(input.toPascalCase, expected)
  }

  test("Camel case to pascal case") {
    val expected = "MyCase"
    val input = "myCase"
    assertEquals(input.toPascalCase, expected)
  }

  test("Snake case to pascal case") {
    val expected = "MyCase"
    val input = "my_case"
    assertEquals(input.toPascalCase, expected)
  }

  test("Kebab case to pascal case") {
    val expected = "MyCase"
    val input = "my-case"
    assertEquals(input.toPascalCase, expected)
  }

  test("Sentence to pascal case") {
    val expected = "MyCase"
    val input = "My Case"
    assertEquals(input.toPascalCase, expected)
  }

  test("Numbers in text to pascal case") {
    val expected = "My2Case"
    val input = "My 2 Case"
    assertEquals(input.toPascalCase, expected)
  }

  test("Snake case to snake case") {
    val expected = "my_case"
    val input = "my_case"
    assertEquals(input.toSnakeCase, expected)
  }

  test("Pascal case to snake case") {
    val expected = "my_case"
    val input = "MyCase"
    assertEquals(input.toSnakeCase, expected)
  }

  test("Camel case to snake case") {
    val expected = "my_case"
    val input = "myCase"
    assertEquals(input.toSnakeCase, expected)
  }

  test("Kebab case to snake case") {
    val expected = "my_case"
    val input = "my-case"
    assertEquals(input.toSnakeCase, expected)
  }

  test("Sentence to snake case") {
    val expected = "my_case"
    val input = "My Case"
    assertEquals(input.toSnakeCase, expected)
  }

  test("Numbers in text to snake case") {
    val expected = "my_2_case"
    val input = "My 2 Case"
    assertEquals(input.toSnakeCase, expected)
  }
}
