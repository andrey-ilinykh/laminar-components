package lc.model

trait TreeNode[+R] {
  def row: R
  def children: Seq[TreeNode[R]]
}
