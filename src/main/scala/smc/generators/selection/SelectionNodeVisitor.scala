package smc.generators.selection

trait SelectionNodeVisitor {
  def visit(node: FsmClassNode): Unit
  def visit(node: EventDelegatorNode): Unit
  def visit(node: StateFieldNode): Unit
  def visit(node: HandleEventNode): Unit
  def visit(node: SwitchCaseNode): Unit
  def visit(node: CaseNode): Unit
  def visit(node: DefaultCaseNode): Unit
  def visit(node: FunctionCallNode): Unit
  def visit(node: NextStateNode): Unit
  def visit(node: EnumNode): Unit
  def writeFiles(path: String, fileName: String): Unit
}
