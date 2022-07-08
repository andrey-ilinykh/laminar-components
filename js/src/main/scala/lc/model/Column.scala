package lc.model

case class Column[R](columnName: String, columnData: R => String)
