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

class LDPatch[Rdf <: RDF](implicit val ops: RDFOps[Rdf]) {

  import ops._

  object grammar {

    import scala.util.parsing.combinator._
    import java.io._
  
    val uri = """[^\u0000-\u0020<>"{}|^`\\]*""".r
    val INTEGER = """[0-9]+""".r
    val name = """[a-zA-Z][a-zA-Z0-9_-]*|[a-zA-Z_][a-zA-Z0-9_]+""".r

    class LDPatchParser(baseURI: Rdf#URI, var prefixes: Map[String, Rdf#URI] = Map.empty)
    extends RegexParsers with JavaTokenParsers with PackratParsers {

      def ldpatch: Parser[List[Statement[Rdf]]] =
        prologue ~ rep(statement) ^^ { case prefixes ~ statements => statements }

      def statement: Parser[Statement[Rdf]] = (add/* | delete*/) 

      def prologue: Parser[Unit] = rep(prefix) ^^^ ()

      def prefix: Parser[Unit] =
        "Prefix" ~ name ~ ":" ~ "<" ~ iri ~ ">" ^^ { case "Prefix" ~ qname ~ ":" ~ "<" ~ uri ~ ">" => this.prefixes += (qname -> uri) }

      def add: Parser[Add[Rdf]] =
        "Add" ~ subject ~ predicate ~ objectt ^^ { case "Add" ~ s ~ p ~ o => Add(s, p, o) }

      def subject: Parser[Subject[Rdf]] = (
          qnameORuri ^^ { PatchIRI(_) }
        | bnode ^^ { PatchBNode(_) }
        | varr
      )

      def predicate: Parser[Predicate[Rdf]] =
        qnameORuri ^^ { PatchIRI(_) }

      def objectt: Parser[Objectt[Rdf]] = (
          qnameORuri ^^ { PatchIRI(_) }
        | bnode ^^ { PatchBNode(_) }
        | literal ^^ { PatchLiteral(_) }
        | varr
      )


      def delete: Parser[Delete[Rdf]] = ???

      def iri: Parser[Rdf#URI] = ( IRIREF | PrefixNamed )

      def IRIREF: Parser[Rdf#URI] =
        "<" ~ uri ~ ">" ^^ { case "<" ~ uri ~ ">" => baseURI.resolve(URI(uri)) }

      def PrefixNamed: Parser[Rdf#URI] = (
          PNAME_LN ^^ { case (prefix, localName) => URI(prefix + localName) }
        | PNAME_NS ^^ { case prefix => URI(prefix) }
      )

      def PNAME_NS: Parser[String] = PN_PREFIX.? ~ ':' ^^ {
        case prefixOpt ~ ':' => prefixOpt.getOrElse(baseURI.getString)
      }

      def PNAME_LN: Parser[(String, String)] = PNAME_NS ~ PN_LOCAL ^^ {
        case prefix ~ localName => (prefix, localName)
      }

      def bnode: Parser[Rdf#BNode] = (
          "_:" ~ name ^^ { case "_:" ~ name => BNode(name) }
        | "[]" ^^ { _ => BNode() }
      )

      def literal: Parser[Rdf#Literal] = (
          stringLiteral ~ opt("^^" ~ qnameORuri) ^^ {
            case lit ~ Some("^^" ~ dt) => Literal(lit.substring(1, lit.size - 1), dt)
            case lit ~ None            => Literal(lit.substring(1, lit.size - 1), xsd.string)
          }
        | INTEGER ^^ { i => Literal(i, xsd.integer) }
      )

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

      def asChar(r: String): Parser[Char] = asChar(r.r)

      lazy val PN_CHARS_BASE: Parser[Char] = asChar("""[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]""") // \u10000-\uEFFFF
      lazy val PN_CHARS_U: Parser[Char] = ( PN_CHARS_BASE | accept('_') )
      lazy val PN_CHARS: Parser[Char] = ( PN_CHARS_U | asChar("""[-0-9\u00B7\u0300-\u036F\u203F-\u2040]""") )
      lazy val PN_PREFIX: Parser[String] = PN_CHARS_BASE ~ ((PN_CHARS | accept('.')).* ~ PN_CHARS ).? ^^ {
        case firstChar ~ charsOpt =>
          val sb = new StringBuffer
          sb.append(firstChar)
          charsOpt.foreach { case chars ~ lastChar =>
            chars.foreach(char => sb.append(char))
            sb.append(lastChar)
          }
          sb.toString()
      }



      lazy val PN_LOCAL: Parser[String] = ???
//        (PN_CHARS_U | asChar("[:0-9]".r) | PLX) ~ ((PN_CHARS | asChar("[.:]") | PLX).* ~ (PN_CHARS | ':' | PLX)).?
      lazy val PLX: Parser[String] = PERCENT | PN_LOCAL_ESC
      lazy val PERCENT: Parser[String] = '%' ~ HEX ~ HEX ^^ { case '%' ~ h1 ~ h2 => s"%$h1$h2" }
      lazy val HEX: Parser[Char] = asChar("[0-9A-Fa-f]")
      lazy val PN_LOCAL_ESC: Parser[String] = "\\[_~.\\-!$&\'()*+,;=/?#@%]".r

    }

  }

}
