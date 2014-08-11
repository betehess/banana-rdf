package org.w3.banana.ldpatch

import org.w3.banana._
import org.w3.banana.ldpatch.model._
import org.scalatest._

abstract class SandrosChallenge[Rdf <: RDF]()(
  implicit ops: RDFOps[Rdf],
  reader: RDFReader[Rdf, Turtle])
extends WordSpec with Matchers { self =>

  import ops._

  val baseURI = "http://example.com/"

  def parseGraph(input: String): Rdf#Graph = reader.read(input, baseURI).get

  val g = new Grammar[Rdf] { implicit val ops = self.ops }

  def parseLDPatch(input: String): model.LDPatch[Rdf] = g.grammar.parseLDPatch(input, URI(baseURI)).get

  val s = new Semantics[Rdf] { implicit val ops = self.ops }

  def applyLDPatch(ldpatch: model.LDPatch[Rdf], graph: Rdf#Graph): Rdf#Graph = s.semantics.LDPatch(ldpatch, graph)

  "test1" in {
    val graph = parseGraph("""<alice> <knows> <bob>, <charlie>.""")
    val expectedGraph = parseGraph("""<alice> <knows> <bob>, <dave>.""")
    val ldpatch = parseLDPatch("""
Delete <alice> <knows> <charlie> .
Add    <alice> <knows> <dave> .
""")
    applyLDPatch(ldpatch, graph).isIsomorphicWith(expectedGraph) should be (true)
  }


  // an example on rdf:list. Great! But this shines with more complex
  // examples, especially when compared with SPARQL. For example: long
  // lists, replacing a slice, appending at nth position, appending at
  // the end of the list without knowing its size.
  //
  // Also, if your lists are natively implemented (eg. if you're using
  // JSON-LD in MongoDB) then you have the same performance
  // characteristics than with arrays.
  "test2" in {
    val graph = parseGraph("""<alice> <knows> ( <bob> <charlie> ).""")
    val expectedGraph = parseGraph("""<alice> <knows> ( <bob> <dave> ).""")
    // this is replacing the 1-nth element (it's 0-indexed)
    val ldpatch1 = parseLDPatch("""UpdateList <alice> <knows> 1>2 ( <dave> ) .""")
    applyLDPatch(ldpatch1, graph).isIsomorphicWith(expectedGraph) should be (true)
    // this is replacing the entire list after the 1-nth element (only
    // one element, but just showing off)
    val ldpatch2 = parseLDPatch("""UpdateList <alice> <knows> 1>  ( <dave> ) .""")
    applyLDPatch(ldpatch2, graph).isIsomorphicWith(expectedGraph) should be (true)
  }


  "test3" in {
    val graph = parseGraph("""<alice> <knows> [ <knows> <bob> ], [<knows> <charlie>].""")
    val expectedGraph = parseGraph("""<alice> <knows> [ <knows> <bob> ], [<knows> <dave>].""")
    // a diff algorithm would have probably started with <charlie> and
    // found that only one hop was needed to bind to the bnode.
    val ldpatch = parseLDPatch("""
Bind   ?x <alice> /<knows>[/<knows>=<charlie>] .
Delete ?x <knows> <charlie> .
Add    ?x <knows> <dave> .
""")
    applyLDPatch(ldpatch, graph).isIsomorphicWith(expectedGraph) should be (true)
  }


  "test4" in {
    val graph = parseGraph("""
<alice> <knows>
   [ <name> "Bob" ],
   [ <name> "Charlie"].
""")
    val expectedGraph = parseGraph("""
<alice> <knows>
   [ <name> "Bob" ],
   [ <name> "Dave"].
""")
    // the serialization for RDF nodes is exactly the same than in
    // Turtle, and therefore the same than in SPARQL. That's actually
    // where most of the complexity in the parser implementation is.
    // 
    // unlike SPARQL, equality is strict: no complex stuff with the
    // language tags, etc.
    val ldpatch = parseLDPatch("""
Bind   ?x <alice> /<knows>[/<name>="Charlie"] .
Delete ?x <name> "Charlie" .
Add    ?x <name> "Dave" .
""")
    applyLDPatch(ldpatch, graph).isIsomorphicWith(expectedGraph) should be (true)
  }


  "test5" in {
    val graph = parseGraph("""
[ a <Order>;
  <items> (
     [ <code> "4343"; <count> 1 ]
     [ <code> "4344"; <count> 3 ]
     [ <code> "4347"; <count> 3 ]
  );
  <shipTo> [
     a <Address>;
     <street> [ <num> 32; <name> "Vassar St" ];
     <city> "Cambridge";
     <state> "MA";
     <zip> 02139
  ];
  <billTo> [
     a <Address>;
     <street> [ <num> 32; <name> "Vassar St" ];
     <city> "Cambridge";
     <state> "MA";
     <zip> 02139
  ]
].
""")
    val expectedGraph = parseGraph("""
[ a <Order>;
  <items> (
     [ <code> "4343"; <count> 1 ]
     [ <code> "4344"; <count> 2 ]
     [ <code> "4347"; <count> 3 ]
  );
  <shipTo> [
     a <Address>;
     <street> [ <num> 32; <name> "Vassar St" ];
     <city> "Cambridge";
     <state> "MA";
     <zip> 02139
  ];
  <billTo> [
     a <Address>;
     <street> [ <num> 36; <name> "Vassar St" ];
     <city> "Cambridge";
     <state> "MA";
     <zip> 02139
  ]
].
""")
    // here showing the shorter instructions, variable overwriting,
    // and nested path constraints. Reminder: there are executed *in
    // order*.
    val ldpatch = parseLDPatch("""
B ?x "4344" /-<code> .
D ?x <count> 3 .
A ?x <count> 2 .

B ?x 32 /-<num>[/-<street>[/-<billTo>]] .
D ?x <num> 32 .
A ?x <num> 36 .
""")
    applyLDPatch(ldpatch, graph).isIsomorphicWith(expectedGraph) should be (true)
  }

  // I am not sure what the point of this example was. But this is
  // clearly not a pathological graph.
  "test6" in {
    val graph = parseGraph("""
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 1 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 2 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 3 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 4 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 5 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 6 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 7 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 8 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 9 ]]]]]]]]].
""")
    val expectedGraph = parseGraph("""
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 1 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 2 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 3 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 4 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 5 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 6 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 7 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 8 ]]]]]]]]].
<node> <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> [ <p> 0 ]]]]]]]]].
""")
    val ldpatch = parseLDPatch("""
B ?x 9 /-<p> .
D ?x <p> 9 .
A ?x <p> 0 .
""")
    applyLDPatch(ldpatch, graph).isIsomorphicWith(expectedGraph) should be (true)
  }

  // as per [1], the input graph is a pathological graph: no grounded
  // node to begin with.
  // 
  // [1] https://dvcs.w3.org/hg/ldpwg/raw-file/ldpatch/ldpatch.html#pathological-graph
  "test7" in {
    val graph = parseGraph("""
_:x <a> _:y.
_:y <a> _:z.
_:z <a> _:x.
""")
    val expectedGraph = parseGraph("""
_:x <a> _:y.
_:y <a> _:z.
_:x <a> _:z.
""")
    // you can't use LDPatch for such a graph
  }


  // this is also a pathological graph as this fails the second
  // characteristic from [1]: there is no unique path from <node> to
  // (one of) the bnodes you want to bind.
  //
  // TimBL would say: this fails the reverse functional property test.
  // 
  // As we said several times, it is pretty easy to find such RDF
  // graphs where LD Patch cannot be used. We gave them a name and
  // tried to characterize them. It is important for people to report
  // such pathological graphs observed in nature. But please, let's
  // avoid SPARQL applications and focus on Linked Data ones...
  // 
  // also, the given graphs were not valid Turtle and tried to guess
  // the intention in fixing them, but I don't think it will change
  // anything.
  "test8" in {
    val graph = parseGraph("""
<node> <p> [ <p> [ <p> [ <p> "1" ],
                       [ <p> "1" ]] ,
                 [ <p> [ <p> "1" ]]],
           [ <p> [ <p> [ <p> "1" ]  ,
                       [ <p> "1" ]]],
           [ <p> [ <p> [ <p> "1" ]]].
""")
    val expectedGraph = parseGraph("""
<node> <p> [ <p> [ <p> [ <p> "1" ],
                       [ <p> "1" ]] ,
                 [ <p> [ <p> "1" ]]],
           [ <p> [ <p> [ <p> "1" ]  ,
                       [ <p> "1" ], [ <p> "1" ]]],
           [ <p> [ <p> [ <p> "1" ]]].
""")
    // you can't use LDPatch for such a graph
  }


}

import org.w3.banana.jena._

class SandrosChallengeWithJena extends SandrosChallenge[Jena]
