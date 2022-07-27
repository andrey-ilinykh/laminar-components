package lc.model

import lc.view.TC

sealed trait TableEvent[+R, +K] {

}

case class CellClicked[R, K](row: R, key: K, column: TC[R]) extends TableEvent[R, K]
case class CellSelected[R, K](row: R, key: K, column: TC[R]) extends TableEvent[R, K]
case class Action[R, K](row: R, key: K, column: TC[R]) extends TableEvent[R, K]
