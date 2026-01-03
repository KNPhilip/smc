package smc

extension (input: String)
  private def words: Seq[String] =
    input.replaceAll("([a-z0-9])([A-Z])", "$1 $2")
         .replaceAll("[_\\-\\s]+", " ")
         .trim
         .split("\\s+")
         .toSeq
         .filter(_.nonEmpty)
         .map(_.toLowerCase)

  def toCamelCase: String =
    words match
      case Nil => ""
      case head +: tail =>
        head + tail.map(_.capitalize).mkString

  def toPascalCase: String =
    words.map(_.capitalize).mkString

  def toSnakeCase: String =
    words.mkString("_")
