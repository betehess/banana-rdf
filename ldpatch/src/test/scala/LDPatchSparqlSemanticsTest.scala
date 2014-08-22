package org.w3.banana.ldpatch

import org.w3.banana.{ Delete => _, _ }
import org.scalatest.{ Filter => _, _ }
import java.io._
import scala.util.{ Try, Success, Failure }
import org.w3.banana.ldpatch.model._

abstract class LDPatchSparqlSemanticsTest[Rdf <: RDF]()(
  implicit ops: RDFOps[Rdf],
  reader: RDFReader[Rdf, Turtle],
  writer: RDFWriter[Rdf, Turtle],
  SparqlOps: SparqlOps[Rdf],
  SparqlUpdateGraph: SparqlUpdateGraph[Rdf]
) extends WordSpec with /*Must*/Matchers with TryValues { self =>

  import ops._
  val baseURI = "http://example.com/"
  def parseGraph(input: String): Rdf#Graph = reader.read(input, baseURI).get
  def write(graph: Rdf#Graph): Unit = println(writer.asString(graph, baseURI).get)
  val g = new Grammar[Rdf] { implicit val ops = self.ops }
  def parseLDPatch(input: String): model.LDPatch[Rdf] = g.grammar.parseLDPatch(input, URI(baseURI)).get
  val s = new SparqlSemantics[Rdf] {
    implicit val ops = self.ops
    implicit val SparqlOps = self.SparqlOps
    implicit val SparqlUpdateGraph = self.SparqlUpdateGraph
  }
  def applyLDPatch(ldpatch: model.LDPatch[Rdf], graph: Rdf#Graph): Rdf#Graph = s.semantics.LDPatch(ldpatch)(graph).get

  val specGraph = parseGraph("""
@prefix schema: <http://schema.org/> .
@prefix profile: <http://ogp.me/ns/profile#> .
@prefix ex: <http://example.org/vocab#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
<http://example.com/timbl#> a schema:Person ;
  schema:alternateName "TimBL" ;
  profile:first_name "Tim" ;
  profile:last_name "Berners-Lee" ;
  schema:workLocation [ schema:name "W3C/MIT" ] ;
  schema:attendee _:b1, _:b2 ;
  ex:preferredLanguages ( "en" "fr" ).

_:b1 schema:name "F2F5 - Linked Data Platform" ;
  schema:url <https://www.w3.org/2012/ldp/wiki/F2F5> .

_:b2 a schema:Event ;
  schema:name "TED 2009" ;
  schema:startDate "2009-02-04" ;
  schema:url <http://conferences.ted.com/TED2009/> .
""")

  val graph = parseGraph("""
<foo> <p> "foo" .
[ <p> "foo" ; <p2> "bar" ]
""")

  "test1" in {
    val expectedGraph = parseGraph("""
[ <p> "foo" ; <p2> "bar" ]
""")
    val ldpatch = parseLDPatch("""
Delete <foo> <p> "foo" .
""")
    applyLDPatch(ldpatch, graph).isIsomorphicWith(expectedGraph) should be (true)
  }


//  "Path semantics" in {
//
//    val path = newParser("""/schema:attendee[/schema:url=<http://conferences.ted.com/TED2009/>]/schema:name""").Path.run().success.value
//
//    val nodes = s.semantics.Path(path, URI("http://example.com/timbl#"), s.semantics.State(graph))
//
//    nodes should be (Set(Literal("TED 2009")))
//
//  }
//
//
//  "Path semantics (exception)" in {
//
//    val path = newParser("""/schema:attendee!""").Path.run().success.value
//
//    a [RuntimeException] should be thrownBy {
//
//      s.semantics.Path(path, URI("http://example.com/timbl#"), s.semantics.State(graph))
//
//    }
//
//  }
//
//  "UpdateList semantics" in {
//
//    val ul = newParser("""UpdateList <#> ex:preferredLanguages 1>2 ( "fr-CH" ) .""").UpdateList.run().success.value
//
//    import org.w3.banana.diesel._
//
//    val newGraph = s.semantics.UpdateList(ul, s.semantics.State(graph)).graph
//
//    val l = (PointedGraph(URI("http://example.com/timbl#"), newGraph) / ex("preferredLanguages")).as[List[String]].success.value
//
//    l should be (List("en", "fr-CH"))
//
//  }
//
//
//  def testUpdate(initialList: List[Int], update: String, expectedList: List[Int]) = {
//
//    import org.w3.banana.diesel._
//
//    val graph: Rdf#Graph = (URI("http://example.com/#") -- ex("numbers") ->- initialList).graph
//
//    val ul = newParser(update).UpdateList.run().success.value
//
//    val newGraph = s.semantics.UpdateList(ul, s.semantics.State(graph)).graph
//
//    val list = (PointedGraph(URI("http://example.com/#"), newGraph) / ex("numbers")).as[List[Int]].success.value
//
//    list should be (expectedList)
//
//  }
//
//  "UpdateList semantics2" in {
//
//    // substitution
//
//    testUpdate(List(1, 2), """UpdateList <http://example.com/#> ex:numbers 1>2 ( 42 ) .""", List(1, 42))
//
//    testUpdate(List(1, 2), """UpdateList <http://example.com/#> ex:numbers 1>2 ( 42 2868 ) .""", List(1, 42, 2868))
//
//    testUpdate(List(1, 2, 3), """UpdateList <http://example.com/#> ex:numbers 1>2 ( 42 2868 ) .""", List(1, 42, 2868, 3))
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 1>3 ( 42 2868 ) .""", List(1, 42, 2868, 4))
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 0>4 ( ) .""", List())
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 4>5 ( 42 ) .""", List(1, 2, 3, 4, 42))
//
//    // append
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers > ( 42 2868 ) .""", List(1, 2, 3, 4, 42, 2868))
//
//    testUpdate(List(), """UpdateList <http://example.com/#> ex:numbers > ( 42 2868 ) .""", List(42, 2868))
//
//    testUpdate(List(), """UpdateList <http://example.com/#> ex:numbers > ( ) .""", List())
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers > ( ) .""", List(1, 2, 3, 4))
//
//    // insert at
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 2>2 ( 42 2868 ) .""", List(1, 2, 42, 2868, 3, 4))
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 0>0 ( 42 2868 ) .""", List(42, 2868, 1, 2, 3, 4))
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 2>2 ( ) .""", List(1, 2, 3, 4))
//
//    // replace everything after
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 2> ( 42 2868 ) .""", List(1, 2, 42, 2868))
//
//    testUpdate(List(), """UpdateList <http://example.com/#> ex:numbers 0> ( 42 2868 ) .""", List(42, 2868))
//
//    testUpdate(List(), """UpdateList <http://example.com/#> ex:numbers 2> ( 42 2868 ) .""", List(42, 2868))
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 4> ( 42 2868 ) .""", List(1, 2, 3, 4, 42, 2868))
//
//    // weird cases
//
//    testUpdate(List(1, 2, 3, 4), """UpdateList <http://example.com/#> ex:numbers 2>1 ( 42 2868 ) .""", List(1, 2, 42, 2868, 3, 4))
//
//  }
//
//
//  "full test" in {
//
//    val patch = """
//@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
//@prefix schema: <http://schema.org/> .
//@prefix profile: <http://ogp.me/ns/profile#> .
//@prefix ex: <http://example.org/vocab#> .
//
//Delete <#> profile:first_name "Tim" .
//Add    <#> profile:first_name "Timothy" .
//
//UpdateList <#> ex:preferredLanguages 1>2 ( "fr-CH" ) .
//
//Bind ?event <#> /schema:attendee[/schema:url = <https://www.w3.org/2012/ldp/wiki/F2F5>]  .
//Add ?event rdf:type schema:Event .
//
//Bind ?ted <http://conferences.ted.com/TED2009/> /-schema:url! .
//Delete ?ted schema:startDate "2009-02-04".
//Add ?ted schema:location _:loc .
//Add _:loc schema:name "Long Beach, California" .
//Add _:loc schema:geo _:geo .
//Add _:geo schema:latitude "33.7817" .
//Add _:geo schema:longitude "-118.2054" .
//"""
//
//    val expectedGraph = reader.read("""
//@prefix schema: <http://schema.org/> .
//@prefix profile: <http://ogp.me/ns/profile#> .
//@prefix ex: <http://example.org/vocab#> .
//@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
//<http://example.com/timbl#> a schema:Person ;
//  schema:alternateName "TimBL" ;
//  profile:first_name "Timothy" ;
//  profile:last_name "Berners-Lee" ;
//  schema:workLocation [ schema:name "W3C/MIT" ] ;
//  schema:attendee _:b1, _:b2 ;
//  ex:preferredLanguages ( "en" "fr-CH" ).
//
//_:b1 a schema:Event ;
//  schema:name "F2F5 - Linked Data Platform" ;
//  schema:url <https://www.w3.org/2012/ldp/wiki/F2F5> .
//
//_:b2 a schema:Event ;
//  schema:name "TED 2009" ;
//  schema:url <http://conferences.ted.com/TED2009/> ;
//  schema:location [
//    schema:name "Long Beach, California";
//    schema:geo [ schema:latitude "33.7817" ; schema:longitude "-118.2054" ]
//  ] .
//""", "http://example.com/timbl").success.value
//
//    val ldpatch = newFreshParser(patch).LDPatch.run().success.value
//
//    val newGraph = s.semantics.LDPatch(ldpatch, graph)
//
//  }



}

import org.w3.banana.jena._

class JenaLDPatchSparqlSemanticsTest extends LDPatchSparqlSemanticsTest[Jena]
