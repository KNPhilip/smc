package smc.generators.selection

sealed trait SelectionNode {
  def accept(visitor: SelectionNodeVisitor): Unit
}

final case class FsmClassNode(className: String, delegators: EventDelegatorNode, eventEnum: EnumNode,
                              stateEnum: EnumNode, stateProperty: StateFieldNode, handleEvent: HandleEventNode,
                              actions: List[String], states: List[String]) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)
}

final case class EventDelegatorNode(events: List[String]) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)
}

final case class StateFieldNode(initialState: String) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)
}

final case class HandleEventNode(switchCase: SwitchCaseNode) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)
}

final case class SwitchCaseNode(variableName: String,
                                caseNodes: List[SelectionNode] = Nil
                               ) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)

  def generateCases(visitor: SelectionNodeVisitor): Unit =
    caseNodes.foreach(_.accept(visitor))
}

final case class CaseNode(switchName: String, caseName: String,
                          caseActionNode: Option[SelectionNode] = None
                         ) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)
}

final case class DefaultCaseNode(state: String) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)
}

final case class FunctionCallNode(functionName: String,
                                  argument: Option[SelectionNode] = None
                                 ) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)
}

final case class NextStateNode(nextState: String) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)
}

final case class EnumNode(name: String, enumerators: List[String]
                         ) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    visitor.visit(this)
}

final case class CompositeNode(nodes: List[SelectionNode] = Nil) extends SelectionNode {
  override def accept(visitor: SelectionNodeVisitor): Unit =
    nodes.foreach(_.accept(visitor))

  def add(node: SelectionNode): CompositeNode =
    copy(nodes = nodes :+ node)
}
