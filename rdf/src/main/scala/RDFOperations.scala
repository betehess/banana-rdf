package org.w3.rdf

/**
 * A Module that gathers the types needed to define an RDF implementation
 * Closely based on
 *   http://dvcs.w3.org/hg/rdf/raw-file/default/rdf-concepts/index.html
 * But with the  notable exceptions:
 *   - we allow literals in subject position - for help later in reasoning.
 */
trait RDFOperations[Rdf <: RDF] {

  // that's really neat: if you import the content of an instance of this trait,
  // you'll get all the type aliases for free, so you don't need the type projection :-)
  type Graph = Rdf#Graph
  type Triple = Rdf#Triple
  type Node = Rdf#Node
  type IRI = Rdf#IRI
  type BNode = Rdf#BNode
  type Literal = Rdf#Literal
  type TypedLiteral = Rdf#TypedLiteral
  type LangLiteral = Rdf#LangLiteral
  type Lang = Rdf#Lang

  trait GraphCompanionObject {
    def empty: Graph
    def apply(elems: Triple*): Graph
    def apply(it: Iterable[Triple]): Graph
    def toIterable(graph: Graph): Iterable[Triple]
  }
  val Graph: GraphCompanionObject

  trait TripleCompanionObject extends Function3[Node, IRI, Node, Triple] {
    def unapply(t: Triple): Option[(Node, IRI, Node)]
  }
  
  val Triple: TripleCompanionObject

  trait NodeCompanionObject {
    def fold[T](node: Node)(funIRI: IRI => T, funBNode: BNode => T, funLiteral: Literal => T): T
  }
  
  val Node: NodeCompanionObject
  
  trait IRICompanionObject extends Function1[String, IRI] {
    def unapply(i: IRI): Option[String]
  }
  
  val IRI : IRICompanionObject

  trait BNodeCompanionObject extends Function1[String, BNode] with Function0[BNode] {
    def unapply(bn: BNode): Option[String]
  }
  
  val BNode: BNodeCompanionObject

  trait LiteralCompanionObject {
    def fold[T](literal: Literal)(funTL: TypedLiteral => T, funLL: LangLiteral => T): T
  }
  
  val Literal: LiteralCompanionObject
  
  trait TypedLiteralCompanionObject extends Function2[String, IRI, TypedLiteral] with Function1[String, TypedLiteral] {
    def unapply(tl: TypedLiteral): Option[(String, IRI)]
    def apply(lexicalForm: String): TypedLiteral = TypedLiteral(lexicalForm, IRI("http://www.w3.org/2001/XMLSchema#string"))
  }
  
  val TypedLiteral: TypedLiteralCompanionObject
  
  trait LangLiteralCompanionObject extends Function2[String, Lang, LangLiteral] {
    def unapply(ll: LangLiteral): Option[(String, Lang)]
  }
  
  val LangLiteral: LangLiteralCompanionObject
  
  trait LangCompanionObject extends Function1[String, Lang] {
    def unapply(l: Lang): Option[String]
  }
  
  val Lang: LangCompanionObject

  // pimps
  
  class GraphW(graph: Graph) {
    def toIterable: Iterable[Triple] = Graph.toIterable(graph)
  }
  
  implicit def wrapGraph(graph: Graph): GraphW = new GraphW(graph)
  implicit def graphAsIterable(graph: Graph): Iterable[Triple] = Graph.toIterable(graph)
  
  implicit def tupleToTriple(tuple: (Node, IRI, Node)): Triple = Triple(tuple._1, tuple._2, tuple._3)

  class TripleW(triple: Triple) {
    val Triple(subject, predicate, objectt) = triple
  }
  implicit def wrapTriple(triple: Triple): TripleW = new TripleW(triple)

  class NodeW(node: Node) {
    def fold[T](funIRI: IRI => T, funBNode: BNode => T, funLiteral: Literal => T): T =
      Node.fold(node)(funIRI, funBNode, funLiteral)
  }
  
  implicit def wrapNode(node: Node): NodeW = new NodeW(node)
  
  class LiteralW(literal: Literal) {
    def lexicalForm = Literal.fold(literal) (
      { case TypedLiteral(s, _) => s },
      { case LangLiteral(s, _) => s }
    )
    def fold[T](funTL: TypedLiteral => T, funLL: LangLiteral => T): T = Literal.fold(literal)(funTL, funLL)
  }
  
  private val _xsd = XSDPrefix(this)

  implicit def wrapLiteral(literal: Literal): LiteralW = new LiteralW(literal)
  
  implicit def wrapIntAsLiteral(i: Int): TypedLiteral = TypedLiteral(i.toString, _xsd.int)
  
  implicit def wrapStringAsLiteral(s: String): TypedLiteral = TypedLiteral(s, _xsd.string)
  
  implicit def wrapFloatAsLiteral(f: Double): TypedLiteral = TypedLiteral(f.toString, _xsd.double)
  
  class LiteralBuilder(lexicalForm: String) {
    def datatype(datatype: IRI): TypedLiteral = TypedLiteral(lexicalForm, datatype)
    def lang(tag: String): LangLiteral = LangLiteral(lexicalForm, Lang(tag))
    def § = TypedLiteral(lexicalForm, _xsd.string)
  }
  
  implicit def wrapStringInLiteralBuilder(lexicalForm: String): LiteralBuilder = new LiteralBuilder(lexicalForm)

  
}
