package org.w3.banana.ldpatch

import org.w3.banana._
import scala.util.Try

sealed trait Statement[Rdf <: RDF]

case class Add[Rdf <: RDF](s: Subject[Rdf], p: Predicate[Rdf], o: Objectt[Rdf]) extends Statement[Rdf]
//    case class AddList(s: Subject, p: Predicate, list: Listt) extends Statement
case class Delete[Rdf <: RDF](s: Subject[Rdf], p: Predicate[Rdf], o: Objectt[Rdf]) extends Statement[Rdf]
//    case class Replace(s: Subject, p: Predicate, slice: Slice, list: Listt) extends Statement

sealed trait Subject[+Rdf <: RDF]
sealed trait Predicate[+Rdf <: RDF]
sealed trait Objectt[+Rdf <: RDF]

case class PatchIRI[Rdf <: RDF](uri: Rdf#URI) extends Subject[Rdf] with Predicate[Rdf] with Objectt[Rdf]
case class PatchBNode[Rdf <: RDF](bnode: Rdf#BNode) extends Subject[Rdf] with Objectt[Rdf]
case class PatchLiteral[Rdf <: RDF](literal: Rdf#Literal) extends Objectt[Rdf]
case class Var(label: String) extends Subject[Nothing] with Objectt[Nothing]

object LDPatch {
  def apply[Rdf <: RDF](implicit ops: RDFOps[Rdf]): LDPatch[Rdf] = new LDPatch[Rdf]
}

object RicherParserChar {
  import scala.util.parsing.combinator.Parsers
  implicit class RicherParserChar(p: Parsers#Parser[Char]) {
    def ps: Parsers#Parser[String] = p.map(_.toString)
  }
}

//object RicherParserString {
//  import scala.util.parsing.combinator.Parsers
//  implicit class RicherParserString(p: Parsers#Parser[Char]) {
//    def ps: Parsers#Parser[String] = p.map(_.toString)
//  }
//}

class LDPatch[Rdf <: RDF](implicit val ops: RDFOps[Rdf]) {

  import ops._

  object grammar {

    import scala.util.parsing.combinator._
    import java.io._
  
    val INTEGER = "[+-]?[0-9]+".r
    val DECIMAL = "[+-]?[0-9]*\\.[0-9]+".r
    val DOUBLE = "[+-]?([0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.[0-9]+[eE][+-]?[0-9]+|[0-9]+[eE][+-]?[0-9]+"
    val	EXPONENT = "[eE][+-]?[0-9]+".r
    val uri = """[^<>"{}|^`\\]*""".r // """[^\u0000-\u0020<>"{}|^`\\]*""".r
    val name = """[a-zA-Z][a-zA-Z0-9_-]*|[a-zA-Z_][a-zA-Z0-9_]+""".r

    class LDPatchParser(baseURI: Rdf#URI, var prefixes: Map[String, Rdf#URI] = Map.empty)
    extends RegexParsers with /*syntactical.StdTokenParsers  with*/ JavaTokenParsers /*with PackratParsers*/ {

      class Wrap[+T](name:String,parser:Parser[T]) extends Parser[T] {
        def apply(in: Input): ParseResult[T] = {
          val first = in.first
          val pos = in.pos
          val offset = in.offset
          val t = parser.apply(in)
          println(s"$name.apply for token {$first} at position $pos offset $offset returns $t")
          t
        }
      }

      implicit def toWrapped(name:String) = new {
        def !!![T](p:Parser[T]) = new Wrap(name,p)
      }

      def ldpatch: Parser[List[Statement[Rdf]]] =
        prologue ~ rep(statement) ^^ { case prefixes ~ statements => statements }

      def statement: Parser[Statement[Rdf]] = (add/* | delete*/) 

      def prologue: Parser[Unit] = rep(prefix) ^^^ ()

      val WS = "[ \t\r\n]*".r

      def prefix: Parser[Unit] = "prefix" !!! {
        "Prefix" ~> WS ~> PNAME_NS ~ IRIREF <~ WS <~ "." ^^ { case qname ~ uri => this.prefixes += (qname -> uri) }
      }
      def add: Parser[Add[Rdf]] =
        "Add" ~ subject ~ predicate ~ objectt ^^ { case "Add" ~ s ~ p ~ o => Add(s, p, o) }

      def subject: Parser[Subject[Rdf]] = (
          iri ^^ { PatchIRI(_) }
        | bnode ^^ { PatchBNode(_) }
        | varr
      )

      def predicate: Parser[Predicate[Rdf]] =
        iri ^^ { PatchIRI(_) }

      def objectt: Parser[Objectt[Rdf]] = (
          iri ^^ { PatchIRI(_) }
        | bnode ^^ { PatchBNode(_) }
        | literal ^^ { PatchLiteral(_) }
        | varr
      )


      def delete: Parser[Delete[Rdf]] = ???

      def iri: Parser[Rdf#URI] = ( IRIREF | PrefixedName )

      def IRIREF: Parser[Rdf#URI] =
        "<" ~ uri ~ ">" ^^ { case "<" ~ uri ~ ">" => println("$$$$ "+uri); baseURI.resolve(URI(uri)) }

      def PrefixedName: Parser[Rdf#URI] = (
          PNAME_LN ^^ { case (prefix, localName) => URI(prefixes(prefix).getString + localName) }
        | PNAME_NS ^^ { case prefix => URI(prefix) }
      )

      def PNAME_NS: Parser[String] = "PNAME_NS" !!! { PN_PREFIX.? ~ ":" ^^ {
        case prefixOpt ~ ":" => prefixOpt.getOrElse(baseURI.getString)
      }
      }
      def PNAME_LN: Parser[(String, String)] = PNAME_NS ~ PN_LOCAL ^^ {
        case prefix ~ localName => (prefix, localName)
      }

      def bnode: Parser[Rdf#BNode] = (
          "_:" ~ name ^^ { case "_:" ~ name => BNode(name) }
        | "[]" ^^ { _ => BNode() }
      )

      def literal: Parser[Rdf#Literal] = RDFLiteral | NumericLiteral | BooleanLiteral

      def NumericLiteral: Parser[Rdf#Literal] = (
          INTEGER ^^ { lexicalForm => Literal(lexicalForm, xsd.integer) }
        | DECIMAL ^^ { lexicalForm => Literal(lexicalForm, xsd.decimal) }
        | DOUBLE ^^ { lexicalForm => Literal(lexicalForm, xsd.double) }
      )

      def BooleanLiteral: Parser[Rdf#Literal] = (
          "true" ^^^ xsd.`true`
        | "false" ^^^ xsd.`false`
      )

      // RDFLiteral 	::= 	String (LANGTAG | '^^' iri)?
      def RDFLiteral: Parser[Rdf#Literal] = TypedLiteral | LangLiteral

      def TypedLiteral: Parser[Rdf#Literal] = STRING ~ ("^^" ~ iri).? ^^ {
        case lexicalForm ~ None             => Literal(lexicalForm)
        case lexicalForm ~ Some("^^" ~ uri) => Literal(lexicalForm, uri)
      }

      def LangLiteral: Parser[Rdf#Literal] = STRING ~ LANGTAG ^^ {
        case lexicalForm ~ langtag => Literal.tagged(lexicalForm, Lang(langtag))
      }

      def LANGTAG: Parser[String] = "@[a-zA-Z]+(-[a-zA-Z0-9]+)*".r ^^ { _.substring(1) }

      def STRING: Parser[String] = ( STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE )

      def makeString(chars: Iterable[Char]): String = {
        val sb = new StringBuffer
        chars.foreach(c => sb.append(c))
        sb.toString()
      }

      def STRING_LITERAL_QUOTE: Parser[String] = "\"" ~ (oneOf("[^\"\\\n\r]") | ECHAR | UCHAR).* ~ "\"" ^^ { /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
        case "\"" ~ chars ~ "\"" => chars.mkString
      }
      def STRING_LITERAL_SINGLE_QUOTE: Parser[String] = "'" ~ (oneOf("[^'\\\n\r]") | ECHAR | UCHAR).* ~ "'" ^^ { /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
        case "'" ~ chars ~ "'" => chars.mkString
      }
      def STRING_LITERAL_LONG_SINGLE_QUOTE: Parser[String] = "'''" ~ (("'" | "''").? ~ (oneOf("[^'\\]") | ECHAR | UCHAR)).* ~ "'''" ^^ {
        case "'''" ~ chars ~ "'''" =>
          val sb = new StringBuffer
          chars.foreach { case quotesOpt ~ chars =>
            quotesOpt.foreach(c => sb.append(c))
            sb.append(chars.toString)
          }
          sb.toString
      }
      def STRING_LITERAL_LONG_QUOTE: Parser[String] = "\"\"\"" ~ (("\"\"?").? ~ (oneOf("[^\"\\]") | ECHAR | UCHAR)).* ~ "\"\"\"" ^^ {
        case "\"\"\"" ~ chars ~ "\"\"\"" =>
          val sb = new StringBuffer
          chars.foreach { case quotesOpt ~ chars =>
            quotesOpt.foreach(c => sb.append(c))
            sb.append(chars.toString)
          }
          sb.toString
      }
      
      def UCHAR: Parser[Char] = "\\u" ~ "[0-9A-Fa-f]{4}".r /*| '\U' HEX HEX HEX HEX HEX HEX HEX HEX*/ ^^ {
        case "\\u" ~ code => java.lang.Integer.parseInt(code, 16).asInstanceOf[Char]
      }
      def ECHAR: Parser[Char] = "\\[tbnrf\"'\\]".r ^^ {
        case "\\t" => '\t'
        case "\\b" => '\b'
        case "\\n" => '\n'
        case "\\r" => '\r'
        case "\\f" => '\f'
        case "\\\"" => '\"'
        case "\\'" => '\''
        case "\\\\" => '\\'
      }

      def qnameORuri: Parser[Rdf#URI] = (
          "<" ~ uri ~ ">" ^^ {
            case "<" ~ uri ~ ">" => baseURI.resolve(URI(uri))
          }
        | name ~ ":" ~ name ^^ {
          case prefix ~ ":" ~ localName => prefixes.get(prefix) match {
            case Some(uri) => URI(uri + localName)
            case None      => sys.error(s"unknown prefix ${prefix}")
          }
        }
      )

      def varr: Parser[Var] = "?" ~ ident ^^ { case "?" ~ x => Var(x) }

      def asChar(r: scala.util.matching.Regex): Parser[Char] = acceptIf(c => r.findFirstIn(c.toString).isDefined)(c => s"'$c' was not matched by {$r}")

      def oneOf(chars: String): Parser[Char] = asChar(chars.r)

      val PN_CHARS_BASE: Parser[Char] = oneOf("""[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]""") // \u10000-\uEFFFF
      def PN_CHARS_U: Parser[Char] = ( PN_CHARS_BASE | accept('_') )
      def PN_CHARS: Parser[Char] = ( PN_CHARS_U | oneOf("""[-0-9\u00B7\u0300-\u036F\u203F-\u2040]""") )
      def PN_PREFIX: Parser[String] = "PN_PREFIX" !!! {PN_CHARS_BASE ~ PN_PREFIX_AUX.? ^^ {
        case firstChar ~ charsOpt =>
          val sb = new StringBuffer
          sb.append(firstChar)
          charsOpt.foreach { chars => sb.append(chars) }
          sb.toString()
      }}
      def PN_PREFIX_AUX: Parser[String] = "PN_PREFIX_AUX" !!! {(PN_CHARS | accept('.')).+ ^^ {
        case chars =>
          val sb = new StringBuffer
          chars.foreach(char => sb.append(char))
          sb.toString()
      } filter (s => s.charAt(s.length-1) != '.')
      }

      def PN_LOCAL: Parser[String] = (PN_CHARS_U | oneOf("[:0-9]") | PLX) ~ PN_LOCAL_AUX.? ^^ {
        case first ~ restOpt =>
          val sb = new StringBuffer
          first match {
            case char: Char => sb.append(char)
            case chars: String => sb.append(chars)
          }
          restOpt.foreach { chars => sb.append(chars) }
          sb.toString
      }
      def PN_LOCAL_AUX: Parser[String] = (PN_CHARS | accept(':') | PLX).+ ^^ {
        case chars =>
          val sb = new StringBuffer
          chars.foreach {
            case char: Char => sb.append(char)
            case chars: String => sb.append(chars)
          }
          sb.toString()
      } filter (s => s.charAt(s.length-1) != '.')

      def PLX: Parser[String] = PERCENT | PN_LOCAL_ESC
      def PERCENT: Parser[String] = '%' ~ HEX ~ HEX ^^ { case '%' ~ h1 ~ h2 => s"%$h1$h2" }
      def HEX: Parser[Char] = oneOf("[0-9A-Fa-f]")
      def PN_LOCAL_ESC: Parser[String] = "\\[_~.\\-!$&\'()*+,;=/?#@%]".r

    }

  }

}
