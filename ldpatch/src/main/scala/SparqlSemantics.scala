package org.w3.banana.ldpatch

import org.w3.banana._
import scala.util.Try
import org.w3.banana.ldpatch.{ model => m }

trait SparqlUpdateGraph[Rdf <: RDF] {

  def apply(graph: Rdf#Graph, query: Rdf#UpdateQuery): Try[Rdf#Graph]

}

object SparqlUpdateGraph {

  import org.w3.banana.jena._

  implicit object JenaSparqlUpdateGraph extends SparqlUpdateGraph[Jena] {

    import com.hp.hpl.jena.rdf.model.{ Model, ModelFactory }
    import com.hp.hpl.jena.update._
    import com.hp.hpl.jena.query.Dataset
    import com.hp.hpl.jena.sparql.modify.GraphStoreBasic
    import com.hp.hpl.jena.sparql.core.DatasetImpl

    val ops = RDFOps[Jena]

    def apply(graph: Jena#Graph, query: Jena#UpdateQuery): Try[Jena#Graph] = Try {
      // what a dance...
      val clone = ops.makeGraph(ops.graphToIterable(graph))
      val model: Model = ModelFactory.createModelForGraph(graph)
      val dataset: Dataset = new DatasetImpl(model)
      val graphStore: GraphStore = new GraphStoreBasic(dataset)
      val processor: UpdateProcessor = UpdateExecutionFactory.create(query, graphStore)
      processor.execute()
      graphStore.getDefaultGraph()
    }
  }

}

trait SparqlSemantics[Rdf <: RDF] {

  implicit val ops: RDFOps[Rdf]

  implicit val SparqlOps: SparqlOps[Rdf]

  implicit val SparqlUpdateGraph: SparqlUpdateGraph[Rdf]

  import ops._

  object semantics {

    import scalaz.Show
    import scalaz.syntax.show._

    case class SparqlUpdate(operations: List[Operation]) {

      def apply(graph: Rdf#Graph): Try[Rdf#Graph] = {
        println("@@", this.shows)
        val update = SparqlOps.UpdateQuery(this.shows)
        SparqlUpdateGraph(graph, update)
      }

    }

    object SparqlUpdate {
//      implicit val show: Show[SparqlUpdate] = Show.shows(_.operations.mkString("\n"))
      implicit val show: Show[SparqlUpdate] = Show.shows(update => update.operations.map(_.shows).mkString("\n"))
    }

    sealed trait Operation

    object Operation {
      implicit val show: Show[Operation] = Show.shows {
        case insert @ Insert(_, _) => insert.shows
        case delete @ Delete(_, _) => delete.shows
      }
    }

    case class Insert(triple: TripleVar, where: Union) extends Operation

    object Insert {
      implicit val show: Show[Insert] = Show.shows[Insert] { insert =>
        import insert._
        if (where.bgps.isEmpty) {
          s"""INSERT DATA { ${triple} }"""
        } else {
          ???
        }
      }
    }

    case class Delete(triple: TripleVar, where: Union) extends Operation

    object Delete {
      implicit val show: Show[Delete] = Show.shows { delete =>
        import delete._
        if (where.bgps.isEmpty) {
          s"""DELETE DATA { ${triple.shows} }"""
        } else {
          ???
        }
      }
    }


    case class TripleVar(s: m.VarOrConcrete[Rdf], p: Rdf#URI, o: m.VarOrConcrete[Rdf])

    object TripleVar {
      import ops._

      private [this] val _nodeShow: Show[Rdf#Node] = Show.shows(_.fold(
        { case URI(uri) => s"<$uri>" },
        bnode => bnode.toString, // ???
        literal => literal.toString
      ))
      implicit def nodeShow[T <: Rdf#Node]: Show[T] = _nodeShow.asInstanceOf[Show[T]]
      implicit val showVarOrConcrete: Show[m.VarOrConcrete[Rdf]] = Show.shows {
        case m.Concrete(node) => node.shows
        case m.Var(label)     => s"?$label"
      }
      implicit val show: Show[TripleVar] = Show.shows { triple =>
        import triple._
        s"${s.shows} ${p.shows} ${o.shows}"
      }
    }

    case class BGP(list: List[TripleVar])

    case class Union(bgps: List[BGP])

    type PathConstraints = Map[m.Var, BGP]

    def LDPatch(patch: m.LDPatch[Rdf]): SparqlUpdate = {
      val operations = Statements(patch.statements.toList, Map.empty)
      SparqlUpdate(operations)
    }

    def Statements(statements: List[m.Statement[Rdf]], pathConstraints: PathConstraints): List[Operation] = statements match {
      case Nil => Nil
      case statement :: statements => Statement(statement, pathConstraints) match {
        case Left(operations) => operations ++ Statements(statements, pathConstraints)
        case Right(newConstraints) => Statements(statements, newConstraints)
      }
    }

    def Statement(statement: m.Statement[Rdf], pathConstraints: PathConstraints): Either[List[Operation], PathConstraints] = statement match {
      case add@m.Add(_, _, _)          => ???
      case addList@m.AddList(_, _, _)  => ???
      case delete@m.Delete(_, _, _)    => Left(List(Delete(delete, pathConstraints)))
      case bind@m.Bind(_, _, _)        => ???
      case ul@m.UpdateList(_, _, _, _) => ???
    }


    def Add(add: m.Add[Rdf], pathConstraints: PathConstraints): Insert = ???
// {
//      val m.Add(s, p, o) = add
//      val State(graph, varmap) = state
//      val groundTriple = Triple(VarOrConcrete(s, varmap), p, VarOrConcrete(o, varmap))
//      State(graph + groundTriple, varmap)
//    }

    def AddList(addList: m.AddList[Rdf], pathConstraints: PathConstraints): List[Insert] = ???
// {
//      val m.AddList(s, p, list) = addList
//      val State(graph, varmap) = state
//      @annotation.tailrec
//      def loop(s: m.VarOrConcrete[Rdf], p: Rdf#URI, list: Seq[m.VarOrConcrete[Rdf]], acc: Set[Rdf#Triple]): Set[Rdf#Triple] = list match {
//        case Seq() =>
//          acc + Triple(VarOrConcrete(s, varmap), p, rdf.nil)
//        case head +: rest =>
//          val bnode = BNode()
//          val newAcc = acc + Triple(VarOrConcrete(s, varmap), p, bnode) + Triple(bnode, rdf.first, VarOrConcrete(head, varmap))
//          loop(m.Concrete(bnode), rdf.rest, rest, newAcc)
//      }
//      val triples = loop(s, p, list, Set.empty)
//      State(graph union Graph(triples), varmap)
//    }

    def UpdateList(updateList: m.UpdateList[Rdf], pathConstraints: PathConstraints): List[Operation] = ???
// {
//      val State(graph, varmap) = state
//
//      val m.UpdateList(s, p, slice, list) = updateList
//
//      val headList: Rdf#Node = ops.getObjects(graph, VarOrConcrete(s, varmap), p).to[List] match {
//        case Nil         => sys.error(s"[UpdateList] $s $p ?? did not match any triple")
//        case head :: Nil => head
//        case _           =>  sys.error(s"[UpdateList] $s $p ?? did not match a unique triple")
//      }
//
//      val groundList: Seq[Rdf#Node] = list.map(vc => VarOrConcrete(vc, varmap))
//
//      val groundS = VarOrConcrete(s, varmap)
//
//      val (left, right) = slice match {
//        case m.Range(leftIndex, rightIndex) => (Some(leftIndex), Some(rightIndex))
//        case m.EverythingAfter(index)       => (Some(index), None)
//        case m.End                          => (None, None)
//      }
//
//      @annotation.tailrec
//      def step1(s: Rdf#Node, p: Rdf#URI, cursor: Rdf#Node, steps: Option[Int]): (Rdf#Node, Rdf#URI, Rdf#Node) =
//        if (steps.exists(_ <= 0) || cursor == rdf.nil) {
//          (s, p, cursor)
//        } else {
//          ops.getObjects(graph, cursor, rdf.rest).to[List] match {
//            case Nil         => sys.error(s"[UpdateList/step1] out of rdf:list")
//            case next :: Nil =>
//              val elem = ops.getObjects(graph, cursor, rdf.first).headOption.getOrElse(sys.error("[UpdateList/step1] no rdf:first"))
//              step1(cursor, rdf.rest, next, steps.map(_ - 1))
//            case _           => sys.error("[UpdateList/step1] malformed list: more than one element after bnode rdf:rest")
//          }
//
//        }
//
//      val (sLeft, pLeft, oLeft) = step1(groundS, p, headList, left)
//
//      @annotation.tailrec
//      def step2(cursor: Rdf#Node, triplesToRemove: List[Rdf#Triple], steps: Option[Int]): (Rdf#Node, List[Rdf#Triple]) =
//        if (steps.exists(_ <= 0) || cursor == rdf.nil) {
//          (cursor, triplesToRemove)
//        } else {
//          ops.getObjects(graph, cursor, rdf.rest).to[List] match {
//            case Nil         => sys.error(s"[UpdateList/step2] out of rdf:list")
//            case next :: Nil =>
//              val elem = ops.getObjects(graph, cursor, rdf.first).headOption.getOrElse(sys.error("[UpdateList/step2] no rdf:first"))
//              step2(next, Triple(cursor, rdf.first, elem) :: Triple(cursor, rdf.rest, next) :: triplesToRemove, steps.map(_ - 1))
//            case _           => sys.error("[UpdateList/step2] malformed list: more than one element after bnode rdf:rest")
//          }
//        }
//
//      val (headRestList, triplesToRemove) = step2(oLeft, List(Triple(sLeft, pLeft, oLeft)), right.flatMap(r => left.map(l => r - l)))
//
//      @annotation.tailrec
//      def step3(s: Rdf#Node, p: Rdf#URI, headRestList: Rdf#Node, triplesToAdd: List[Rdf#Triple], nodes: Seq[Rdf#Node]): List[Rdf#Triple] = nodes match {
//
//        case Seq() =>
//          Triple(s, p, headRestList) :: triplesToAdd
//            
//        case node +: restNodes =>
//          val bnode = BNode()
//          step3(bnode, rdf.rest, headRestList, Triple(s, p, bnode) :: Triple(bnode, rdf.first, node) :: triplesToAdd, restNodes)
//
//      }
//
//      val triplesToAdd = step3(sLeft, pLeft, headRestList, List.empty, groundList)
//
//      State(graph.diff(Graph(triplesToRemove)).union(Graph(triplesToAdd)), varmap)
//
//    }


    def Delete(delete: m.Delete[Rdf], pathConstraints: PathConstraints): Delete = {
      val m.Delete(s, p, o) = delete
      Delete(TripleVar(s, p, o), Union(List.empty))

    }
// {
//      
//      val State(graph, varmap) = state
//      val groundTriple = Triple(VarOrConcrete(s, varmap), p, VarOrConcrete(o, varmap))
//      State(graph diff Graph(groundTriple), varmap)
//    }

//    def VarOrConcrete(vcn: m.VarOrConcrete[Rdf], varmap: Map[m.Var, Rdf#Node]): Rdf#Node = vcn match {
//      case m.Concrete(node) => node
//      case varr@m.Var(_)    => varmap(varr)
//    }

    def Bind(bind: m.Bind[Rdf], pathConstraints: PathConstraints): PathConstraints = ???
// {
//      val m.Bind(varr, startingNode, path) = bind
//      val State(graph, varmap) = state
//      val nodes = Path(path, VarOrConcrete(startingNode, varmap), state)
//      nodes.size match {
//        case 0 => sys.error(s"$bind didn't match any node")
//        case 1 => State(graph, varmap + (varr -> nodes.head))
//        case n => sys.error(s"$bind matched $n nodes. Required exactly one 1")
//      }
//    }

    def Path(path: m.Path[Rdf], startingNode: Rdf#Node, pathConstraints: PathConstraints): PathConstraints = ???
// {
//
//      val State(graph, varmap) = state
//
//      def PathElement(pathElem: m.PathElement[Rdf], nodes: Set[Rdf#Node]): Set[Rdf#Node] = pathElem match {
//        case m.StepForward(uri) => nodes.flatMap(node => ops.getObjects(graph, node, uri))
//
//        case m.StepBackward(uri) => nodes.flatMap(node => ops.getSubjects(graph, uri, node))
//
//        case m.StepAt(index) =>
//          @annotation.tailrec
//          def loop(nodes: Set[Rdf#Node], index: Int): Set[Rdf#Node] = index match {
//            case 0 => nodes
//            case i => loop(nodes.flatMap(node => ops.getObjects(graph, node, ops.rdf.rest)), i-1)
//          }
//          loop(nodes, index)
//
//        case m.Filter(path, None) => nodes.filter(node => Path(path, node, state).nonEmpty)
//
//        case m.Filter(path, Some(value)) => nodes.filter { node =>
//          val groundValue = VarOrConcrete(value, varmap)
//          Path(path, node, state) == Set(groundValue)
//        }
//
//        case m.UnicityConstraint => nodes.size match {
//          case 0 => sys.error("failed unicity constraint: got empty set")
//          case 1 => nodes
//          case n => sys.error("failed unicity constraint: got $n matches")
//        }
//
//      }
//
//      path.elems.foldLeft(Set(startingNode))((nodes, pathElem) => PathElement(pathElem, nodes))
//
//    }

  }


}
