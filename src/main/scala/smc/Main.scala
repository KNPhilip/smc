package smc

@main
def main(args: String*): Unit =
    println(s"Args: ${args.mkString(", ")}")

class Calculator {
    def plus(augend: Int, addend: Int):
        Int = augend + addend
}
