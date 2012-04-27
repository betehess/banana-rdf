package org.w3.rdf

trait Prefix[Rdf <: RDF] extends Function1[String, Rdf#IRI]


object PrefixBuilder {
  def apply[Rdf <: RDF](prefix: String, ops: RDFOperations[Rdf]) = new PrefixBuilder(prefix, ops)
}

class PrefixBuilder[Rdf <: RDF](prefix: String, ops: RDFOperations[Rdf]) extends Prefix[Rdf] {
  import ops._
  def apply(value: String): Rdf#IRI = IRI(prefix+value)
}



object RDFPrefix {
  def apply[Rdf <: RDF](ops: RDFOperations[Rdf]) = new RDFPrefix(ops)
}

class RDFPrefix[Rdf <: RDF](ops: RDFOperations[Rdf]) extends PrefixBuilder("http://www.w3.org/1999/02/22-rdf-syntax-ns#", ops) {
  val langString = apply("langString")
  val typ = apply("type")
  val first = apply("first")
  val rest = apply("rest")
  val nil = apply("nil")
}




object XSDPrefix {
  def apply[Rdf <: RDF](ops: RDFOperations[Rdf]) = new XSDPrefix(ops)
}

class XSDPrefix[Rdf <: RDF](ops: RDFOperations[Rdf]) extends PrefixBuilder("http://www.w3.org/2001/XMLSchema#", ops) {
  import ops._

  val string = apply("string")
  val int = apply("int")
  val integer = apply("integer")
  val decimal = apply("decimal")
  val double = apply("double")
  val boolean = apply("boolean")
  val trueLit: Rdf#TypedLiteral = TypedLiteral("true", boolean)
  val falseLit: Rdf#TypedLiteral = TypedLiteral("false", boolean)
}


object DcPrefix {
  def apply[Rdf <: RDF](ops: RDFOperations[Rdf]) = new DcPrefix(ops)
}

class DcPrefix[Rdf <: RDF](ops: RDFOperations[Rdf]) extends PrefixBuilder("http://purl.org/dc/elements/1.1/", ops) {

}




object FOAFPrefix {
  def apply[Rdf <: RDF](ops: RDFOperations[Rdf]) = new FOAFPrefix(ops)
}

class FOAFPrefix[Rdf <: RDF](ops: RDFOperations[Rdf]) extends PrefixBuilder("http://xmlns.com/foaf/0.1/", ops) {
  val name = apply("name")
  val title = apply("title")
  val knows = apply("knows")
  val currentProject = apply("currentProject")
  val Person = apply("Person")
  val age = apply("age")
  val height = apply("height")
  val mbox = apply("mbox")
  val publication = apply("publication")
  val wants = apply("wants")
  val author = apply("author")
}
