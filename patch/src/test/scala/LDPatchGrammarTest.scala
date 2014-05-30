package org.w3.banana.ldpatch

import org.w3.banana._
import org.scalatest._
import java.io._
import scala.util.{ Try, Success, Failure }

abstract class LDPatchGrammarTest[Rdf <: RDF]()(implicit ops: RDFOps[Rdf]) extends WordSpec with Matchers {

  import ops._

  val ldpatch = LDPatch[Rdf]
  import ldpatch.grammar._

//  "parse variable" in {
//    val parser = new PatchParserCombinator[Rdf]
//    val v = parser.parse(parser.varr, new StringReader("?foo"))
//    v.get should be (Var("foo"))
//  }
//
//  "parse literal" in {
//    val parser = new PatchParserCombinator[Rdf](prefixes = Map(xsd.prefixName -> xsd.prefixIri))
//    parser.parse(parser.literal, new StringReader("4")).get should be(TypedLiteral("4", xsd.integer))
//    parser.parse(parser.literal, new StringReader(""""foo"""")).get should be(TypedLiteral("foo", xsd.string))
//    parser.parse(parser.literal, new StringReader(""""foo"^^xsd:string""")).get should be(TypedLiteral("foo", xsd.string))
//
//  }

  def newParser(input: String) =
    new PEGPatchParser(input, baseURI = URI("http://example.com/foo#"), prefixes = Map("foaf" -> URI("http://xmlns.com/foaf/")))

  "parse uri" in {
    val parser = newParser("""<http://example.com/foo#\u2665>""")
    parser.IRIREF.run() should be(Success(URI("http://example.com/foo#♥")))
//    parser.parse(parser.qnameORuri, new StringReader("""<http://example.com>""")).get should be(URI("http://example.com"))
//    parser.parse(parser.qnameORuri, new StringReader("""xsd:foo""")).get should be(xsd("foo"))
  }

  "parse Prefix" in {
    val parser = 
    newParser("""Prefix example: <http://example.com/foo#>""").Prefix.run() should be(Success("example" -> URI("http://example.com/foo#")))
//    println(newParser("example").PN_PREFIX.run())
//    parser.parse(parser.qnameORuri, new StringReader("""<http://example.com>""")).get should be(URI("http://example.com"))
//    parser.parse(parser.qnameORuri, new StringReader("""xsd:foo""")).get should be(xsd("foo"))
  }


}

import org.w3.banana.jena._

class JenaLDPatchGrammarTest extends LDPatchGrammarTest[Jena]
