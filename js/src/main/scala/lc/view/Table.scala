package lc.view

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import lc.model._
import org.scalajs.dom
import org.scalajs.dom.html



sealed trait CellRenderer[C] {
  def render(c: C): Seq[Modifier[ReactiveHtmlElement[dom.html.Element]]]
}

sealed trait ActionRenderer {
  def render(label: String): Seq[Modifier[ReactiveHtmlElement[dom.html.Element]]]
}

trait TC[R] {
  //type C
  def name: String

  //  def extract: R => C
  //
  //  def renderer: CellRenderer[C]
}

case class ActionColumn[R](name: String, label: String, renderer: ActionRenderer) extends TC[R] {
  //  type C = Unit
  //
  //  override def extract: R => Unit = _ => ()
}


case class TableColumn[R, CC:Ordering](name: String, extract: R => CC, ascending: Option[Boolean] = None, renderer: CellRenderer[CC]) extends TC[R] with Ordering[R]{
  override def compare(x: R, y: R): Int = {
    val ord = implicitly[Ordering[CC]]
    ascending match {
      case Some(true) =>
        ord.compare(extract(x), extract(y))
      case Some(false) =>
        - ord.compare(extract(x), extract(y))
      case _ => 0
    }
  }
}

case class TableConfig[R, K](key: R => K, columns: Seq[TC[R]] = Seq()) {
  def withColumn(tc: TC[R]): TableConfig[R, K] = copy(columns = this.columns :+ tc)

  def withColumn[C: CellRenderer:Ordering](name: String, f: R => C, ascending: Option[Boolean] = None): TableConfig[R, K] = {
    withColumn(TableColumn(name, f, ascending, implicitly[CellRenderer[C]]))
  }

  def action(name: String, label: String) =
    withColumn(ActionColumn(name, label, ActionRenderer.outlineButton))
}

object ActionRenderer {
  val outlineButton = new ActionRenderer {
    override def render(label: String): Seq[Modifier[ReactiveHtmlElement[html.Element]]] =
      Seq(
        cls := Seq(Styles.btnDanger.htmlClass, Styles.button.htmlClass),
        label,
      )
  }
}

object CellRenderer {
  implicit val stringRenderer = new CellRenderer[String] {
    override def render(c: String): Seq[Modifier[ReactiveHtmlElement[html.Element]]] =
      Seq(new TextNode(c))
  }

  implicit val intRenderer = new CellRenderer[Int] {
    override def render(c: Int): Seq[Modifier[ReactiveHtmlElement[html.Element]]] =
      Seq(new TextNode(c.toString))
  }

  implicit val boolRenderer = new CellRenderer[Boolean] {
    override def render(c: Boolean): Seq[Modifier[ReactiveHtmlElement[html.Element]]] =
      Seq(input(typ := "checkbox", checked := c))
  }


}

case class Item[R, K](row: R, key: K, pKey: Option[K], expanded: Boolean, hasChildren: Boolean, level: Int)

case class IS[R, K](data: Map[K, Item[R, K]]) {
  def merge(tnSeq: Seq[TreeNode[R]], keyFn: R => K): IS[R, K] = {
    def doMerge(pItem: Option[Item[R, K]], seq: Seq[TreeNode[R]], map: Map[K, Item[R, K]]): Map[K, Item[R, K]] = {
      seq.foldLeft(map) {
        case (m, tn) =>
          val k =  keyFn(tn.row)
          val ni = map.get(k).map(item => item.copy(row = tn.row)).getOrElse(Item(tn.row, k, pItem.map(_.key), false, tn.children.nonEmpty, pItem.map(_.level +1).getOrElse(0)))

          doMerge(Some(ni), tn.children, m + (k -> ni))
      }
    }

    val tmp = doMerge(None, tnSeq, data)
    IS(tmp)
  }

  def seqOrdering(implicit ord: Ordering[R]) = new Ordering[Seq[R]] {
    override def compare(x: Seq[R], y: Seq[R]): Int = {
      val zip = x.zip(y)

      def doCompare(seq: Seq[(R, R)]): Int = {
        if (seq.isEmpty) {
          0
        } else {
          val head = seq.head
          if (head._1 == head._2)
            doCompare(seq.tail)
          else
            ord.compare(head._1, head._2)
        }
      }

      val tmp = doCompare(zip)
      if (tmp == 0)
        x.length - y.length
      else
        tmp
    }
  }


  def flatten( implicit ordering: Ordering[R]): Seq[Item[R, K]] = {
    val closedItems = data.values.filter(!_.expanded).map(_.key).toSet
    def isClosed(pKey: Option[K]): Boolean = {
      pKey match {
        case None => false
        case Some(pKey) =>
            if(closedItems.contains(pKey))
              true
            else
              isClosed(data.get(pKey).flatMap(_.pKey))
      }
    }
    def buildSortKey(i: Item[R, K], sortKey:Seq[R]): Seq[R] = {
      i.pKey match {
        case None => i.row +: sortKey
        case Some(pKey) =>
          buildSortKey(data(pKey), i.row +: sortKey)
      }
    }
    data.values.filter(i => !isClosed(i.pKey)).toSeq.sortBy(i => buildSortKey(i, Seq()))(seqOrdering)
  }

  def flop(k: K) = {

    data.get(k).fold {
      this
    } {
      item => IS(data + (k -> item.copy(expanded = !item.expanded)))
    }

  }
}

object Table {



 def noordering[R] = new Ordering[R] {
    override def compare(x: R, y: R): Int = 0
  }


  import scalacss.ProdDefaults._
  import scalacss.internal.mutable.GlobalRegistry

  GlobalRegistry.addToDocumentOnRegistration()
  GlobalRegistry.register(Styles)


  def treeTable[R, K](tc: TableConfig[R, K], data: Signal[Seq[TreeNode[R]]], observer: Option[Observer[TableEvent[R, K]]] = None)/*(implicit ord: Ordering[K])*/ = {
    val empty: Map[K, Item[R, K]] = Map()
    val state: Var[IS[R, K]] = Var(IS(empty))
    //val selectedRow: Var[Option[K]] = Var(None)
    val selectedCell: Var[Option[CellSelected[R, K]]] = Var(None)
    val ord: Ordering[R] = tc.columns.collect{ case ord: TableColumn[_, _] => ord}.headOption.getOrElse(noordering)
    val flatData = state.signal.map(is => is.flatten(ord))


    def renderRow(key: K, tn: Item[R, K], signal: Signal[Item[R, K]]): ReactiveHtmlElement[html.TableRow] = {


      def renderCell(r: R, c: TC[R]) =
        c match {
          case TableColumn(_, extract, _, renderer) =>
            val mods = (renderer.render(extract(r)) :+ (onClick.mapTo(Some(CellSelected(r, key, c))) --> selectedCell)) //++
            mods
          case ActionColumn(_, l, renderer) =>
            val mods = renderer.render(l)
            val omods: Seq[Modifier[ReactiveHtmlElement[dom.html.Element]]] = observer.map { o =>
              Seq(
                onClick.mapTo(Some(CellSelected(r, key, c))) --> selectedCell,
                onClick.mapTo(Action(r, key, c)) --> o
              )
            }.getOrElse(Seq())
            //val selmod = observer.map(o => Seq(onClick.mapTo(CellSelected(r, key, c)) --> o)).getOrElse(Seq())
            Seq(
              button(
                mods ++ omods
              ),
              onClick.mapTo(Some(CellSelected(r, key, c))) --> selectedCell,
            ) //++ selmod

        }

      def renderHeadCell(r: R, tc: Option[TC[R]]) = {
        def renderCaret(i: Item[R, K]) ={
          if(i.hasChildren){
            (if (i.expanded) Utils.caretDown else Utils.caretRight)
              .amend(onClick.mapTo(()) --> Observer[Unit] { _ =>
                state.set(state.now().flop(key))
              } )
          } else
            Utils.caretEmpty
        }
        val mods = Seq(
          paddingLeft := s"${tn.level}em",
          child <-- signal.map(i => renderCaret(i)),
          onDblClick.mapTo(0) --> Observer[Int] { _ =>
            state.set(state.now().flop(key))

          })
        val cellMods = tc.map(c => renderCell(r, c)).getOrElse(Seq())
        td(
          mods ++ cellMods
        )
      }

      tr(
        //cls <-- selectedRow.signal.map(_.map(sr => if (sr == tn.key(tn.key.length - 1)) Styles.selectedRow.htmlClass else "").getOrElse("")),
        cls <-- selectedCell.signal.map(_.map(sr => if (sr.key == tn.key) Styles.selectedRow.htmlClass else "").getOrElse("")),
        child <-- signal.map { r =>
          renderHeadCell(r.row, tc.columns.headOption)
        },

        children <-- signal.map { r =>
          tc.columns.tail.map { c =>
            td(renderCell(r.row, c))
          }
        }

      )
    }

    table(
      observer.map( o => selectedCell.signal.changes.collect{case Some(evt) => evt} --> o) ,
      cls := Seq(Styles.tableBordered.htmlClass, Styles.table.htmlClass),
      data.map(nList => state.now().merge(nList, tc.key)) --> Observer(x => state.set(x)),
      thead(
        tr(
          tc.columns.map { c =>
            th(c.name)
          }
        )
      ),
      tbody(
        children <-- flatData.split(i => i.key)(renderRow)
      )
    )
  }

  def renderSimpleTable[T](columns: Seq[Column[T]], signal: Signal[Seq[T]]) = {
    table(
      thead(
        tr(
          columns.map { c =>
            th(c.columnName)
          }
        )
      ),
      tbody(
        children <-- signal.map { seq =>
          seq.map { t =>
            tr(
              columns.map { c =>
                td(c.columnData(t))
              }
            )
          }
        }
      )
    )
  }

  def renderTable[R, K](tc: TableConfig[R, K], s: Signal[Seq[R]], observer: Option[Observer[TableEvent[R, K]]] = None) = {
    val signal = s.map(_.zipWithIndex)
    val selectedRow: Var[Option[Int]] = Var(None)

    def renderRow(key: K, row: (R, Int), signal: Signal[(R, Int)]) = {
      def renderCell(r: (R, Int), c: TC[R]) =
        c match {
          case TableColumn(_, extract, _, renderer) =>
            val mods = (renderer.render(extract(r._1)) :+ (onClick.mapTo(Some(r._2)) --> selectedRow)) ++
              observer.map { o =>
                Seq(
                  onClick.mapTo(CellSelected(r._1, key, c)) --> o,
                )
              }.getOrElse(Seq())
            td(mods)
          case ActionColumn(_, l, renderer) =>
            val mods = renderer.render(l)
            val omods: Seq[Modifier[ReactiveHtmlElement[dom.html.Element]]] = observer.map { o =>
              Seq(
                onClick.mapTo(CellSelected(r._1, key, c)) --> o,
                onClick.mapTo(Action(r._1, key, c)) --> o
              )
            }.getOrElse(Seq())
            val selmod = observer.map(o => Seq(onClick.mapTo(CellSelected(r._1, key, c)) --> o)).getOrElse(Seq())
            td(
              button(
                mods ++ omods
              ),
              onClick.mapTo(Some(r._2)) --> selectedRow,
              selmod
            )
        }

      tr(
        cls <-- selectedRow.signal.map(_.map(sr => if (sr == row._2) Styles.selectedRow.htmlClass else "").getOrElse("")),
        children <-- signal.map { r =>
          tc.columns.map { c =>
            renderCell(r, c)
          }
        }
      )
    }

    table(cls := Seq(Styles.tableBordered.htmlClass, Styles.table.htmlClass),
      thead(
        tr(
          tc.columns.map { c =>
            th(c.name)
          }
        )
      ),
      tbody(
        children <-- signal.split(x => tc.key(x._1))(renderRow)
      )
    )
  }


  def renderTable[R, K](columns: Seq[Column[R]], key: R => K, signal: Signal[Seq[R]]) = {
    def renderRow(key: K, row: R, signal: Signal[R]) = {
      tr(
        children <-- signal.map { r =>
          columns.map { c =>
            td(
              contentEditable := true,
              c.columnData(r),

            )
          }
        }
      )
    }

    table(
      thead(
        tr(
          columns.map { c =>
            th(c.columnName)
          }
        )
      ),
      tbody(
        children <-- signal.split(key)(renderRow)
      )
    )
  }


}
