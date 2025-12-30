package smc.generators.nsc

sealed trait NscNode {
  def accept(visitor: NscNodeVisitor): Unit
}

final case class SwitchCaseNode(variableName: String,
                                caseNodes: List[NscNode] = Nil
                               ) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)

  def generateCases(visitor: NscNodeVisitor): Unit =
    caseNodes.foreach(_.accept(visitor))
}

final case class CaseNode(switchName: String, caseName: String,
                          caseActionNode: Option[NscNode] = None
                         ) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)
}

final case class DefaultCaseNode(state: String) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)
}

final case class FunctionCallNode(functionName: String,
                                  argument: Option[NscNode] = None
                                 ) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)
}

final case class CompositeNode(nodes: List[NscNode] = Nil) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    nodes.foreach(_.accept(visitor))

  def add(node: NscNode): CompositeNode =
    copy(nodes = nodes :+ node)
}

final case class EnumNode(name: String, enumerators: List[String]
                         ) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)
}

final case class EnumeratorNode(enumeration: String,
                                enumerator: String
                               ) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)
}

final case class StatePropertyNode(initialState: String) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)
}

final case class EventDelegatorsNode(events: List[String]) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)
}

final case class HandleEventNode(switchCase: SwitchCaseNode) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)
}

final case class FsmClassNode(className: String, actionsName: String,
                              delegators: EventDelegatorsNode, eventEnum: EnumNode,
                              stateEnum: EnumNode, stateProperty: StatePropertyNode,
                              handleEvent: HandleEventNode, actions: List[String],
                              states: List[String]) extends NscNode {
  override def accept(visitor: NscNodeVisitor): Unit =
    visitor.visit(this)
}
