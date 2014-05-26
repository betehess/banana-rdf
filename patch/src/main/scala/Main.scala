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

    def parsePrologue(s: String) = {
      val parser = new grammar.LDPatchParser(baseURI)
      parser.parse(parser.prologue, new StringReader(s))
    }
    def parse(s: String) = {
      val parser = new grammar.LDPatchParser(baseURI)
      parser.parse(parser.ldpatch, new StringReader(s))
    }

    println(parsePrologue("""
Prefix foaf: <http://xmlns.com/foaf/>
Prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
"""))

    println(parse("""
Prefix foaf: <http://xmlns.com/foaf/>
Prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
Add <http://bertails.org/alex#> foaf:name "Alexandre"
"""))

  }

}

import org.w3.banana.jena._

object Main extends LDPatchExample[Jena]
