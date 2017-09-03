package com.asyaminor.functional.book.libraries.parser

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    val nullP: Parser[JSON] = string("null") map(_ => JNull)
    val numberP: Parser[JNumber] = "-?(0|\\d*)(\\.\\d*)?((e|E)(+|-)?\\d*)?".r map (str => JNumber(str.toDouble))
    val jStringP: Parser[JString] = "\"w\"".r map(str => JString(str)) // only selecting words
    val jBoolP: Parser[JBool] = string("true") | string("false") map(b => JBool(b.toBoolean))
    val jArrayP: Parser[JArray] = ???
    val jObjectP: Parser[JObject] = ???
    def valueParser(chunk: String): Parser[JSON] = nullP | numberP | jStringP | jBoolP | jArrayP | jObjectP

    ???

    // first
    // start with an object // question: what about whitespace?
    // then parse key value pairs ===> probably many of them
    // parser for key value will need key parser and value parser
    // key parser is basically a regex parser for \"some_name\"
    // value parser something like null | number | string | bool | array | object(which recurse somehow)
  }
}

