package org.w3.banana.ldpatch

import org.w3.banana._
import scala.util.Try
import scala.language.reflectiveCalls

sealed trait Statement[Rdf <: RDF]

case class Add[Rdf <: RDF](s: Subject[Rdf], p: Predicate[Rdf], o: Objectt[Rdf]) extends Statement[Rdf]
case class AddList[Rdf <: RDF](s: Subject[Rdf], p: Predicate[Rdf], list: Seq[Objectt[Rdf]]) extends Statement[Rdf]
case class Delete[Rdf <: RDF](s: Subject[Rdf], p: Predicate[Rdf], o: Objectt[Rdf]) extends Statement[Rdf]
//    case class Replace(s: Subject, p: Predicate, slice: Slice, list: Listt) extends Statement
case class Bind[Rdf <: RDF](varr: Var, startingNode: Value[Rdf], ldpath: LDPath[Rdf]) extends Statement[Rdf]

case class LDPath[Rdf <: RDF](elements: Seq[PathElement[Rdf]])

sealed trait PathElement[+Rdf <: RDF]
case class Forward[Rdf <: RDF](predicate: Predicate[Rdf]) extends PathElement[Rdf]
case class Backward[Rdf <: RDF](predicate: Predicate[Rdf]) extends PathElement[Rdf]
case class Constraint[Rdf <: RDF](ldpath: LDPath[Rdf], valueOpt: Option[Value[Rdf]]) extends PathElement[Rdf]
case class Index(i: Int) extends PathElement[Nothing]
case object UnicityConstraint extends PathElement[Nothing]

sealed trait Subject[+Rdf <: RDF]
sealed trait Predicate[Rdf <: RDF]
sealed trait Objectt[+Rdf <: RDF]
sealed trait Value[+Rdf <: RDF]

case class PatchIRI[Rdf <: RDF](uri: Rdf#URI) extends Subject[Rdf] with Predicate[Rdf] with Objectt[Rdf] with Value[Rdf]
case class PatchBNode[Rdf <: RDF](bnode: Rdf#BNode) extends Subject[Rdf] with Objectt[Rdf]
case class PatchLiteral[Rdf <: RDF](literal: Rdf#Literal) extends Objectt[Rdf] with Value[Rdf]
case class Var(label: String) extends Subject[Nothing] with Objectt[Nothing] with Value[Nothing]

object LDPatch {
  def apply[Rdf <: RDF](implicit ops: RDFOps[Rdf]): LDPatch[Rdf] = new LDPatch[Rdf]
}

class LDPatch[Rdf <: RDF](implicit val ops: RDFOps[Rdf]) {

  import ops._

  object grammar {

    import org.parboiled2._
    import CharPredicate._

    class PEGPatchParser(val input: ParserInput, baseURI: Rdf#URI, var prefixes: Map[String, Rdf#URI] = Map.empty) extends Parser with StringBuilding {

      // LDPatch ::= Prologue Statement*
      def LDPatch: Rule1[Seq[Statement[Rdf]]] = rule {
        Prologue ~> (prefixes => this.prefixes = prefixes) ~ zeroOrMore(Statement) ~ EOI
      }

      // Statement ::= Bind | Add | Delete | Replace | Prefix | Comment
      def Statement: Rule1[Statement[Rdf]] = rule (
        add | delete //Bind | Add | Delete | Replace | Prefix
      )
  
      // Add ::= "Add" Subject Predicate ( Object | List ) '.'
      def add: Rule1[Statement[Rdf]] = rule (
        "Add" ~ WS1 ~ SubjectR ~ WS1 ~ PredicateR ~ WS1 ~ (
            list ~> (Right(_))
          | ObjectR ~> (Left(_))
        ) ~ WS0 ~ '.' ~> ((s: Subject[Rdf], p: Predicate[Rdf], objectOrList: Either[Objectt[Rdf], Seq[Objectt[Rdf]]]) => objectOrList match {
          case Left(o)     => Add(s, p, o)
          case Right(list) => AddList(s, p, list)
        })
      )

      // List ::= '(' Object* ')'
      def list: Rule1[Seq[Objectt[Rdf]]] = rule (
        '(' ~ WS0 ~ zeroOrMore(ObjectR).separatedBy(WS1) ~ WS0 ~ ')'
      )

      // Delete ::= "Delete" Subject Predicate Object '.'
      def delete: Rule1[Delete[Rdf]] = rule (
        "Delete" ~ WS1 ~ SubjectR ~ WS1 ~ PredicateR ~ WS1 ~ ObjectR ~ WS0 ~ '.' ~> ((s: Subject[Rdf], p: Predicate[Rdf], o: Objectt[Rdf]) => Delete(s, p, o))
      )

      // Bind ::= "Bind" Var Value LDPath? '.'
      def bind: Rule1[Bind[Rdf]] = rule (
        "Bind" ~ WS1 ~ VarR ~ WS1 ~ ValueR ~ optional(WS1 ~ ldpath) ~ WS0 ~ '.' ~> ((varr: Var, value: Value[Rdf], ldpathOpt: Option[LDPath[Rdf]]) => Bind(varr, value, ldpathOpt.getOrElse(LDPath(Seq.empty))))
      )

      // LDPath ::= PathElement*
      def ldpath: Rule1[LDPath[Rdf]] = rule (
        zeroOrMore(pathElement) ~> ((pathElems: Seq[PathElement[Rdf]]) => LDPath(pathElems))
      )

      // PathElement ::= Forward | Backward | Constraint | Index | UnicityConstraint
      def pathElement: Rule1[PathElement[Rdf]] = rule (
        unicityConstraint | backward | forward | constraint | index
      )

      def forward: Rule1[Forward[Rdf]] = rule (
        '/' ~ IRI ~> ((uri: Rdf#URI) => Forward(PatchIRI(uri)))
      )

      def backward: Rule1[Backward[Rdf]] = rule (
        "/-" ~ IRI ~> ((uri: Rdf#URI) => Backward(PatchIRI(uri)))
      )

      def constraint: Rule1[Constraint[Rdf]] = rule (
        '[' ~ ldpath ~ optional('=' ~ ValueR) ~ ']' ~> ((ldpath: LDPath[Rdf], valueOpt: Option[Value[Rdf]]) => Constraint(ldpath, valueOpt))
      )

      def index: Rule1[PathElement[Rdf]] = rule (
        '/' ~ capture(oneOrMore(Digit)) ~> ((s: String) => Index(s.toInt))
      )

      def unicityConstraint: Rule1[UnicityConstraint.type] = rule (
        ch('!') ~ push(UnicityConstraint)
      )
      

      // Subject ::= iri | BlankNode | Var
      def SubjectR: Rule1[Subject[Rdf]] = rule (
          IRI ~> (PatchIRI(_))
        | BlankNode ~> (PatchBNode(_))
        | VarR
      )

      // Predicate ::= iri
      def PredicateR: Rule1[Predicate[Rdf]] = rule (
        IRI ~> (PatchIRI(_: Rdf#URI))
      )

      // Object ::= iri | BlankNode | RDFLiteral | Var
      def ObjectR: Rule1[Objectt[Rdf]] = rule (
          IRI ~> (PatchIRI(_))
        | BlankNode ~> (PatchBNode(_))
        | literal ~> (PatchLiteral(_))
        | VarR
      )

      // Value ::= iri | RDFLiteral | Var
      def ValueR: Rule1[Value[Rdf]] = rule (
          IRI ~> (PatchIRI(_))
        | literal ~> (PatchLiteral(_))
        | VarR
      )

      // iri ::= IRIREF | PrefixedName
      def IRI: Rule1[Rdf#URI] = rule (
        IRIREF | PrefixedName
      )
      
      // BlankNode ::= BLANK_NODE_LABEL | ANON
      def BlankNode: Rule1[Rdf#BNode] = rule (
        BLANK_NODE_LABEL | ANON
      )

      // BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
      def BLANK_NODE_LABEL: Rule1[Rdf#BNode] = rule (
        "_:" ~ clearSB() ~ BLANK_NODE_LABEL1 ~ optional(BLANK_NODE_LABEL2) ~ push(BNode(sb.toString()))
      )

      // PN_CHARS_U | [0-9]
      def BLANK_NODE_LABEL1: Rule0 = rule (
          PN_CHARS_U
        | Digit ~ appendSB()
      )

      // (PN_CHARS | '.')* PN_CHARS
      def BLANK_NODE_LABEL2: Rule0 = rule (
        oneOrMore(PN_CHARS) ~ test(lastChar != '.')
      )

      // ANON ::= '[' WS* ']'
      def ANON: Rule1[Rdf#BNode] = rule (
        '[' ~ WS0 ~ ']' ~ push(BNode())
      )

      // Literal ::= RDFLiteral | NumericLiteral | BooleanLiteral
      def literal: Rule1[Rdf#Literal] = rule (
        RDFLiteral | NumericLiteral | BooleanLiteral
      )

      // RDFLiteral ::= String (LANGTAG | '^^' iri)?
      def RDFLiteral: Rule1[Rdf#Literal] = rule (
        StringR ~ optional(LangOrIRI) ~> ((lexicalForm: String, opt: Option[Either[Rdf#Lang, Rdf#URI]]) => opt match {
          case None                  => Literal(lexicalForm)
          case Some(Left(langtag))   => Literal.tagged(lexicalForm, langtag)
          case Some(Right(datatype)) => Literal(lexicalForm, datatype)
        })
      )

      // just the (LANGTAG | '^^' iri) part
      def LangOrIRI: Rule1[Either[Rdf#Lang, Rdf#URI]] = rule (
          LANGTAG ~> ((lang: Rdf#Lang) => Left(lang))
        | "^^" ~ IRI ~> ((datatype: Rdf#URI) => Right(datatype))
      )

      // NumericLiteral ::= INTEGER | DECIMAL | DOUBLE
      def NumericLiteral: Rule1[Rdf#Literal] = rule (
          DOUBLE ~> ((lexicalForm: String) => Literal(lexicalForm, xsd.double))
        | DECIMAL ~> ((lexicalForm: String) => Literal(lexicalForm, xsd.decimal))
        | INTEGER ~> ((lexicalForm: String) => Literal(lexicalForm, xsd.integer))
      )

      // INTEGER ::= [+-]? [0-9]+
      def INTEGER: Rule1[String] = rule (
        capture(optional(anyOf("+-")) ~ oneOrMore(CharPredicate.Digit))
      )

      // DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
      def DECIMAL: Rule1[String] = rule (
        capture(optional(anyOf("+-")) ~ zeroOrMore(CharPredicate.Digit) ~ '.' ~ zeroOrMore(CharPredicate.Digit))
      )

      // DOUBLE ::= [+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
      def DOUBLE: Rule1[String] = rule (
        capture(optional(anyOf("+-")) ~ (
            oneOrMore(CharPredicate.Digit) ~ '.' ~ zeroOrMore(CharPredicate.Digit) ~ EXPONENT
          | '.' ~ oneOrMore(CharPredicate.Digit) ~ EXPONENT
          | oneOrMore(CharPredicate.Digit) ~ EXPONENT
        ))
      )

      // EXPONENT ::= [eE] [+-]? [0-9]+
      def EXPONENT: Rule0 = rule (
        anyOf("eE") ~ optional(anyOf("+-")) ~ oneOrMore(CharPredicate.Digit)
      )

      // BooleanLiteral ::= 'true' | 'false'
      def BooleanLiteral: Rule1[Rdf#Literal] = rule (
          "true" ~ push(xsd.`true`)
        | "false" ~ push(xsd.`false`)
      )

      // the VAR1 from SPARQL:   Var ::= '?' VARNAME
      def VarR: Rule1[Var] = rule (
        '?' ~ VARNAME
      )

      // VARNAME ::= ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
      def VARNAME: Rule1[Var] = rule (
        clearSB() ~ (PN_CHARS_U | CharPredicate.Digit ~ appendSB()) ~ zeroOrMore(
            PN_CHARS_U
          | (CharPredicate.Digit ++ CharPredicate('\u00B7') ++ between('\u0300', '\u036F') ++ between('\u203F', '\u2040')) ~ appendSB()
        ) ~ push(Var(sb.toString))
      )

      // PrefixedName ::= PNAME_LN | PNAME_NS
      def PrefixedName: Rule1[Rdf#URI] = rule (
          PNAME_LN ~> ((prefix, localName) => URI(prefixes(prefix).getString + localName))
        | PNAME_NS ~> (prefix => URI(prefix))
      )

      // PNAME_LN ::= PNAME_NS PN_LOCAL
      def PNAME_LN: Rule2[String, String] = rule (
        PNAME_NS ~ PN_LOCAL
      )

      // PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
      def PN_LOCAL: Rule1[String] = rule (
        clearSB() ~ (PN_CHARS_U | (CharPredicate(':') ++ CharPredicate.Digit) ~ appendSB() | PLX) ~ optional(PN_LOCAL2) ~ push(sb.toString())
      )

      // (PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX)
      // so basically (PN_CHARS | '.' | ':' | PLX)+ with no trailing '.'
      def PN_LOCAL2: Rule0 = rule (
        oneOrMore(PN_CHARS | anyOf(".:") ~ appendSB() | PLX) ~ test(lastChar != '.')
      )

      // PLX ::= PERCENT | PN_LOCAL_ESC
      def PLX: Rule0 = rule (
        PERCENT | PN_LOCAL_ESC
      )

      // PERCENT ::= '%' HEX HEX
      // HEX ::= [0-9] | [A-F] | [a-f]
      def PERCENT: Rule0 = rule (
        '%' ~ appendSB() ~ CharPredicate.HexDigit ~ appendSB() ~ CharPredicate.HexDigit ~ appendSB()
      )

      // PN_LOCAL_ESC ::= '\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')
      def PN_LOCAL_ESC: Rule0 = rule (
        '\\' ~ appendSB() ~ anyOf("_~.-!$&'()*+,;=/?#@%") ~ appendSB()
      )
      
      // String ::= STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
      def StringR: Rule1[String] = rule (
        STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
      )

      // STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'      /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
      def STRING_LITERAL_QUOTE: Rule1[String] = rule (
        '"' ~ clearSB() ~ zeroOrMore(ECHAR | UCHAR | noneOf("\"\\\n\r") ~ appendSB()) ~ '"' ~ push(sb.toString())
      )

      // STRING_LITERAL_SINGLE_QUOTE ::= "'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'"      /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
      def STRING_LITERAL_SINGLE_QUOTE: Rule1[String] = rule (
        '\'' ~ clearSB() ~ zeroOrMore(ECHAR | UCHAR | noneOf("\"\\\n\r") ~ appendSB()) ~ '\'' ~ push(sb.toString())
      )

      // STRING_LITERAL_LONG_SINGLE_QUOTE ::= "'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
      def STRING_LITERAL_LONG_SINGLE_QUOTE: Rule1[String] = rule (
        "'''" ~ clearSB() ~ zeroOrMore(optional('\'' ~ appendSB() ~ optional('\'' ~ appendSB())) ~ (ECHAR | UCHAR | noneOf("'\\") ~ appendSB())) ~ "'''" ~ push(sb.toString())
      )

      // STRING_LITERAL_LONG_QUOTE ::= '"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
      def STRING_LITERAL_LONG_QUOTE: Rule1[String] = rule (
        "\"\"\"" ~ clearSB() ~ zeroOrMore(optional('"' ~ appendSB() ~ optional('"' ~ appendSB())) ~ (ECHAR | UCHAR | noneOf("\"\\") ~ appendSB())) ~ "\"\"\"" ~ push(sb.toString())
      )

      // ECHAR ::= '\' [tbnrf"'\]
      def ECHAR: Rule0 = rule (
        '\\' ~ (
            't'  ~ appendSB('\t')
          | 'b'  ~ appendSB('\b')
          | 'n'  ~ appendSB('\n')
          | 'r'  ~ appendSB('\r')
          | 'f'  ~ appendSB('\f')
          | '\'' ~ appendSB('\"')
          | '\'' ~ appendSB('\'')
          | '\\' ~ appendSB('\\')
        )
      )

      // LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
      def LANGTAG: Rule1[Rdf#Lang] = rule (
        '@' ~ capture(oneOrMore(CharPredicate.Alpha) ~ zeroOrMore('-' ~ oneOrMore(CharPredicate.AlphaNum))) ~> ((langString: String) => Lang(langString))
      )


      def WS: Rule0 = rule { anyOf(" \t\r\n") }
      def WS0: Rule0 = rule { zeroOrMore(WS) }
      def WS1: Rule0 = rule { oneOrMore(WS) }
  
      // Prologue ::= Prefix*
      def Prologue: Rule1[Map[String, Rdf#URI]] = rule { push(this.prefixes) ~ zeroOrMore(Prefix ~> ((prefixes: Map[String, Rdf#URI], prefix) => push(prefixes + prefix))) }
  
      // Prefix ::= "Prefix" PNAME_NS IRIREF
      def Prefix: Rule1[(String, Rdf#URI)] = rule {
        "Prefix" ~ WS1 ~ PNAME_NS ~ WS0 ~ IRIREF ~> ((qname: String, iri: Rdf#URI) => (qname, iri))
      }
  
      // PNAME_NS ::= PN_PREFIX? ':'
      def PNAME_NS: Rule1[String] = rule {
        optional(PN_PREFIX) ~ ':' ~> ((prefixOpt: Option[String]) => push(prefixOpt.getOrElse("")))
      }
  
      // IRIREF ::= '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>' /* #x00=NULL #01-#x1F=control codes #x20=space */
      def IRIREF: Rule1[Rdf#URI] = rule {
        '<' ~ clearSB() ~ zeroOrMore(IRIREF_CHAR) ~ '>' ~ push(URI(sb.toString()))
      }
  
      // matches a Char in [^#x00-#x20<>"{}|^`\] or /uxxxx or /Uxxxxxxxx, and pushes it on the StringBuffer
      def IRIREF_CHAR: Rule0 = rule (
          UCHAR
        | (CharPredicate('\u0000' to '\u0020') ++ CharPredicate("<>\"{}|^`\\")).negated ~ appendSB()
      )
  
      // UCHAR ::= '\\u' HEX HEX HEX HEX | '\\U' HEX HEX HEX HEX HEX HEX HEX HEX
      def UCHAR: Rule0 = rule {
        "\\u" ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> ((code: String) => appendSB(java.lang.Integer.parseInt(code, 16).asInstanceOf[Char]))
      }
  
      // PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
      def PN_PREFIX: Rule1[String] = rule {
        clearSB() ~ PN_CHARS_BASE ~ optional(PN_PREFIX2) ~ push(sb.toString())
      }
  
      // (PN_CHARS | '.')* PN_CHARS
      // so basically PN_CHARS+ with no trailing '.'
      def PN_PREFIX2: Rule0 = rule {
  //      capture(oneOrMore(PN_CHARS)) ~> ((s: String) => test(s.charAt(s.length-1) != '.') ~ appendSB(s))
        oneOrMore(PN_CHARS) ~ test(lastChar != '.')
      }
  
      def between(low: Char, high: Char): CharPredicate = CharPredicate.from(c => c >= low && c <= high)
  
      // PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
      def PN_CHARS_BASE: Rule0 = rule (
          CharPredicate.Alpha ~ appendSB()
        | (between('\u00C0', '\u00D6') ++ between('\u00D8', '\u00F6') ++ between('\u00F8', '\u02FF') ++ between('\u0370', '\u037D') ++ between('\u037F', '\u1FFF') ++ between('\u200C', '\u200D') ++ between('\u2070', '\u218F') ++ between('\u2C00', '\u2FEF') ++ between('\u3001', '\uD7FF') ++ between('\uF900', '\uFDCF') ++ between('\uFDF0', '\uFFFD')) ~ appendSB()
      )
  
      // PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
      def PN_CHARS: Rule0 = rule (
          PN_CHARS_U
        | (CharPredicate('-') ++ CharPredicate.Digit ++ CharPredicate('\u00B7') ++ between('\u0300', '\u036F') ++ between('\u203F', '\u2040')) ~ appendSB()
      )
  
      // PN_CHARS_U ::= PN_CHARS_BASE | '_'
      def PN_CHARS_U: Rule0 = rule (
          PN_CHARS_BASE
        | '_' ~ appendSB()
      )
  
  
    }

  }

}
