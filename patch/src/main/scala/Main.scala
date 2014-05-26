package org.w3.banana.ldpatch

import org.w3.banana._
import scala.util._
import java.io._

class LDPatchExample[Rdf <: RDF](implicit ops: RDFOps[Rdf]) {

  import ops._

  val ldPatch = LDPatch[Rdf]
  import ldPatch._

  def main(args: Array[String]): Unit = {

    val parser = new grammar.LDPatchParser

    def parsePrologue(s: String) = parser.parse(parser.prologue, new StringReader(s))
    def parse(s: String) = parser.parse(parser.ldpatch, new StringReader(s))

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
