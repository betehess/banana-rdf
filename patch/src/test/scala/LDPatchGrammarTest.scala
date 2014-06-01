package org.w3.banana.ldpatch

import org.w3.banana._
import org.scalatest._
import java.io._
import scala.util.{ Try, Success, Failure }

abstract class LDPatchGrammarTest[Rdf <: RDF]()(implicit ops: RDFOps[Rdf]) extends WordSpec with Matchers with TryValues {

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

  "parse IRIREF" in {
    newParser("""<http://example.com/foo#\u2665>""").IRIREF.run().success.value should be(URI("http://example.com/foo#♥"))
  }

  "parse iri" in {
    newParser("""<http://example.com/foo#\u2665>""").IRI.run().success.value should be(URI("http://example.com/foo#♥"))
    newParser("""foaf:name""").IRI.run().success.value should be(URI("http://xmlns.com/foaf/name"))
  }

  "parse Prefix" in {
    newParser("""Prefix example:<http://example.com/foo#>""").Prefix.run().success.value should be("example" -> URI("http://example.com/foo#"))
    newParser("""Prefix   example: <http://example.com/foo#>""").Prefix.run().success.value should be("example" -> URI("http://example.com/foo#"))
    newParser("""Prefix : <http://example.com/foo#>""").Prefix.run().success.value should be("" -> URI("http://example.com/foo#"))
  }

  "parse BlankNode" in {
    newParser("""_:foo""").BlankNode.run().success.value should be(BNode("foo"))
    newParser("""[]""").BlankNode.run() shouldBe a [Success[_]]
    newParser("""[ ]""").BlankNode.run() shouldBe a [Success[_]]
  }

  "parse RDFLiteral" in {
    newParser(""""foo"""").RDFLiteral.run().success.value should be(Literal("foo"))
    newParser(""""foo"@en""").RDFLiteral.run().success.value should be(Literal.tagged("foo", Lang("en")))
    newParser(""""foo"^^foaf:name""").RDFLiteral.run().success.value should be(Literal("foo", URI("http://xmlns.com/foaf/name")))
    newParser(""""foo"^^<http://xmlns.com/foaf/name>""").RDFLiteral.run().success.value should be(Literal("foo", URI("http://xmlns.com/foaf/name")))
  }

  "parse NumericLiteral" in {
    newParser("""42""").NumericLiteral.run().success.value should be(Literal("42", xsd.integer))
    newParser("""-42""").NumericLiteral.run().success.value should be(Literal("-42", xsd.integer))
    newParser("""3.14""").NumericLiteral.run().success.value should be(Literal("3.14", xsd.decimal))
    newParser("""-3.14""").NumericLiteral.run().success.value should be(Literal("-3.14", xsd.decimal))
    newParser("""42e-10""").NumericLiteral.run().success.value should be(Literal("42e-10", xsd.double))
    newParser("""-3.14E10""").NumericLiteral.run().success.value should be(Literal("-3.14E10", xsd.double))    
  }

  "parse BooleanLiteral" in {
    newParser("""true""").BooleanLiteral.run().success.value should be(xsd.`true`)
    newParser("""false""").BooleanLiteral.run().success.value should be(xsd.`false`)
  }

  "parse literal" in {
    newParser(""""foo"^^foaf:name""").literal.run().success.value should be(Literal("foo", URI("http://xmlns.com/foaf/name")))
    newParser("""-3.14""").literal.run().success.value should be(Literal("-3.14", xsd.decimal))
    newParser("""true""").literal.run().success.value should be(xsd.`true`)
  }

  "parse Add Object" in {
    newParser("""Add _:betehess foaf:name "Alexandre Bertails" .""").add.run().success.value should be(
      Add(
        PatchBNode(BNode("betehess")),
        PatchIRI(URI("http://xmlns.com/foaf/name")),
        PatchLiteral(Literal("Alexandre Bertails"))
      )
    )
  }

  "parse Add List" in {
    newParser("""Add _:betehess foaf:name ( "Alexandre Bertails" "Betehess" ) .""").add.run().success.value should be(
      AddList(
        PatchBNode(BNode("betehess")),
        PatchIRI(URI("http://xmlns.com/foaf/name")),
        Seq(PatchLiteral(Literal("Alexandre Bertails")), PatchLiteral(Literal("Betehess")))
      )
    )
  }

  "parse Delete" in {
    newParser("""Delete ?betehess foaf:name "Alexandre Bertails" .""").delete.run().success.value should be(
      Delete(
        Var("betehess"),
        PatchIRI(URI("http://xmlns.com/foaf/name")),
        PatchLiteral(Literal("Alexandre Bertails"))
      )
    )
  }

  "parse LDPath" in {
    newParser("""/foaf:name/-foaf:name/<http://example.com/foo>""").ldpath.run().success.value should be(
      LDPath(Seq(
        Forward(PatchIRI(URI("http://xmlns.com/foaf/name"))),
        Backward(PatchIRI(URI("http://xmlns.com/foaf/name"))),
        Forward(PatchIRI(URI("http://example.com/foo")))
      ))
    )

    newParser("""[/<http://example.com/foo>/foaf:name="Alexandre Bertails"]""").ldpath.run().success.value should be(
      LDPath(Seq(
        Constraint(
          LDPath(Seq(
            Forward(PatchIRI(URI("http://example.com/foo"))),
            Forward(PatchIRI(URI("http://xmlns.com/foaf/name")))
          )),
          Some(PatchLiteral(Literal("Alexandre Bertails")))
        )
      ))
    )

    newParser("""[/<http://example.com/foo>/-foaf:name]/foaf:friend""").ldpath.run().success.value should be(
      LDPath(Seq(
        Constraint(
          LDPath(Seq(
            Forward(PatchIRI(URI("http://example.com/foo"))),
            Backward(PatchIRI(URI("http://xmlns.com/foaf/name")))
          )),
          None
        ),
        Forward(PatchIRI(URI("http://xmlns.com/foaf/friend")))
      ))
    )

    newParser("""/foaf:name!/42""").ldpath.run().success.value should be(
      LDPath(Seq(
        Forward(PatchIRI(URI("http://xmlns.com/foaf/name"))),
        UnicityConstraint,
        Index(42)
      ))
    )

  }


  "parse Bind" in {

    newParser("""Bind ?foo <http://example.com/blah> .""").bind.run().success.value should be(
      Bind(
        Var("foo"),
        PatchIRI(URI("http://example.com/blah")),
        LDPath(Seq())
      )
    )

    newParser("""Bind ?foo <http://example.com/blah> /foaf:name/-foaf:name/<http://example.com/foo> .""").bind.run().success.value should be(
      Bind(
        Var("foo"),
        PatchIRI(URI("http://example.com/blah")),
        LDPath(Seq(
          Forward(PatchIRI(URI("http://xmlns.com/foaf/name"))),
          Backward(PatchIRI(URI("http://xmlns.com/foaf/name"))),
          Forward(PatchIRI(URI("http://example.com/foo")))
        ))
      )
    )

  }

}

import org.w3.banana.jena._

class JenaLDPatchGrammarTest extends LDPatchGrammarTest[Jena]
