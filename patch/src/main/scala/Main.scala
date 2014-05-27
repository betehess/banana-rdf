package org.w3.banana.ldpatch

import org.w3.banana._
import scala.util._
import java.io._

class LDPatchExample[Rdf <: RDF](implicit ops: RDFOps[Rdf]) {

  import ops._

  val ldPatch = LDPatch[Rdf]
  import ldPatch._

  def main(args: Array[String]): Unit = {

    val baseURI = URI("http://example.com/base#")

    import scala.util.parsing.combinator.Parsers

    def newParser() = new grammar.LDPatchParser(baseURI, Map("foaf" -> URI("http://xmlns.com/foaf/")))

    def parsePrologue(s: String) = {
      val parser = new grammar.LDPatchParser(baseURI)
      parser.parse(parser.prologue, new StringReader(s))
      parser.prefixes
    }

    def parse(s: String) = {
      val parser = new grammar.LDPatchParser(baseURI)
      val result = parser.parse(parser.ldpatch, new StringReader(s))
      (result, result.next.source)
    }

    def parseIRIREF(s: String) = {
      val parser = newParser()
      parser.parse(parser.IRIREF, new StringReader(s))
    }

    def parseIri(s: String) = {
      val parser = newParser()
      parser.parse(parser.iri, new StringReader(s))
    }

    def parsePrefixedName(s: String) = {
      val parser = newParser()
      parser.parse(parser.PrefixedName, new StringReader(s))
    }

    def parsePNAME_NS(s: String) = {
      val parser = newParser()
      parser.parse(parser.PNAME_NS, new StringReader(s))
    }

    def parsePrefix(s: String) = {
      val parser = new grammar.LDPatchParser(baseURI)
      parser.parse(parser.prefix, new StringReader(s))
      parser.prefixes
    }

    def parseLiteral(s: String) = {
      val parser = newParser()
      parser.parse(parser.literal, new StringReader(s))
    }

//    def parseFoo(s: String) = {
//      val parser = newParser()
//      parser.parse(parser.foo, new StringReader(s))
//    }

    println(parsePrologue("""
Prefix foaf: <http://xmlns.com/foaf/> .
Prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
"""))

    println(parse("""
Prefix foaf: <http://xmlns.com/foaf/> .
Prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
Add <http://bertails.org/alex#> foaf:name "Alexandre"
"""))

    println(parseIRIREF("<http://example.com/foo>"))
   println(parsePrefixedName("foaf:foo"))

    println(parseLiteral(""""fooooo""""))
    println(parseLiteral(""""fooooo"^^foaf:blah"""))
    println(parseLiteral(""""fooooo"^^<http://www.w3.org/2001/XMLSchema#string>"""))
    println(parseLiteral(""""42dd"^^<http://www.w3.org/2001/XMLSchema#integer>"""))
    println(parseLiteral(""""fooooo"@enfr"""))
    println(parseLiteral("""42"""))
    println(parseLiteral("""true"""))

    println(parseIri("""foaf:blah"""))
    println(parseIri("""<http://example.com/blah>"""))

    println(parsePNAME_NS("foo:"))
println("@@@@@@@@@@@@@@@@@@@@@")
    println(parsePrefix("Prefix foo:<http://example.com/>."))

  }

}

import org.w3.banana.jena._

object Main extends LDPatchExample[Jena]
