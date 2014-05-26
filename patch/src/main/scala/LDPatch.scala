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
  
    val uri = """[a-zA-Z0-9:/#_\.\-\+]+""".r
    val integer = """[0-9]+""".r
    val name = """[a-zA-Z][a-zA-Z0-9_-]*|[a-zA-Z_][a-zA-Z0-9_]+""".r

    class LDPatchParser(var base: Option[Rdf#URI] = None, var prefixes: Map[String, Rdf#URI] = Map.empty)
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




      def iri: Parser[Rdf#URI] = uri ^^ { URI(_) }

      def bnode: Parser[Rdf#BNode] = (
          "_:" ~ name ^^ { case "_:" ~ name => BNode(name) }
        | "[]" ^^ { _ => BNode() }
      )

      def literal: Parser[Rdf#Literal] = (
          stringLiteral ~ opt("^^" ~ qnameORuri) ^^ {
            case lit ~ Some("^^" ~ dt) => Literal(lit.substring(1, lit.size - 1), dt)
            case lit ~ None            => Literal(lit.substring(1, lit.size - 1), xsd.string)
          }
        | integer ^^ { i => Literal(i, xsd.integer) }
      )

      def qnameORuri: Parser[Rdf#URI] = (
          "<" ~ uri ~ ">" ^^ {
            case "<" ~ uri ~ ">" => base match {
              case Some(b) => b.resolve(URI(uri))
              case None    => URI(uri)
            }
          }
        | name ~ ":" ~ name ^^ {
          case prefix ~ ":" ~ localName => prefixes.get(prefix) match {
            case Some(uri) => URI(uri + localName)
            case None      => sys.error(s"unknown prefix ${prefix}")
          }
        }
      )

      def varr: Parser[Var] = "?" ~ ident ^^ { case "?" ~ x => Var(x) }

    }

  }

}
