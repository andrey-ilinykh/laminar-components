package lc.view

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import lc.model._
import org.scalajs.dom
import org.scalajs.dom.html

//case class Col[T](t: T, i: Int = 0, s: Option[String] = None)
//
//trait ColUpdater{
//  def update[T](c: Col[T]): Col[T]
//}
//object colupdater {
//   def iupdate(i: Int): ColUpdater = new ColUpdater {
//     override def update[T](c: Col[T]): Col[T] = c.copy(i = i)
//   }
//
//   def supdate(s: String)(c: Col[_]): Col[_] = c.copy(s = Some(s))
//}
//
//object removeme {
//  def buildCol[T](t: T, cms: ColUpdater[Col] *) = {
//    cms.foldLeft(Col(t)){
//      case (result, u) => u.update(result)
//    }
//  }
//
//  def doIt() ={
//    buildCol[Double](4.0, colupdater.iupdate(23))
//  }
//}

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

trait TableColumnModifier{
  def update[R, C: Ordering](tc: TableColumn[R, C]): TableColumn[R, C]
}

object TableColumnModifier{
  object sorting{
    def asc = new TableColumnModifier{
      override def update[R, C: Ordering](tc: TableColumn[R, C]): TableColumn[R, C] = tc.copy(ascending = Some(true))
    }
    def desc = new TableColumnModifier{
      override def update[R, C: Ordering](tc: TableColumn[R, C]): TableColumn[R, C] = tc.copy(ascending = Some(false))
    }
    def none = new TableColumnModifier{
      override def update[R, C: Ordering](tc: TableColumn[R, C]): TableColumn[R, C] = tc.copy(ascending = None)
    }
  }
}

case class TableConfig[R, K](key: R => K, columns: Seq[TC[R]] = Seq() , selectedRow: Option[Observer[Option[R]]] = None ) {
  //val selectedRow: Var[Option[R]] = Var(None)
  def withColumn(tc: TC[R]): TableConfig[R, K] = copy(columns = this.columns :+ tc)

  def withColumn[C: CellRenderer:Ordering](name: String, f: R => C, ascending: Option[Boolean] = None): TableConfig[R, K] = {
    withColumn(TableColumn(name, f, ascending, implicitly[CellRenderer[C]]))
  }

  def action(name: String, label: String) =
    withColumn(ActionColumn(name, label, ActionRenderer.outlineButton))
}

trait TableConfigModifier[R, K] {
  def modify(tc: TableConfig[R, K]): TableConfig[R, K]
}

object TableConfigModifier {
  def selectedRow[R, K](observer: Observer[Option[R]]) = new TableConfigModifier[R, K] {
    override def modify(tc: TableConfig[R, K]): TableConfig[R, K] = tc.copy(selectedRow = Some(observer))
  }
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
  //type TableModifier[R, K] = TableConfig[R, K] => TableConfig[R, K]

  def column[R, K, C: Ordering: CellRenderer](name: String, extractor: R => C, cms: TableColumnModifier *): TableConfigModifier[R, K] = tc => {
    val col = cms.foldLeft(TableColumn[R, C](name, extractor, None, implicitly[CellRenderer[C]])) {
      case (result, cm) => cm.update(result)
    }
    tc.copy(columns = tc.columns :+ col )
  }

 def noordering[R] = new Ordering[R] {
    override def compare(x: R, y: R): Int = 0
  }


  import scalacss.ProdDefaults._
  import scalacss.internal.mutable.GlobalRegistry

  GlobalRegistry.addToDocumentOnRegistration()
  GlobalRegistry.register(Styles)

  private def renderCell[R, K]( key: K, r: R, c: TC[R])(implicit observer: Option[Observer[TableEvent[R, K]]], selectedCell: Observer[Option[CellSelected[R, K]]]) =
    c match {
      case TableColumn(_, extract, _, renderer) =>
        val mods = (renderer.render(extract(r)) :+ (onClick.mapTo(Some(CellSelected(r, key, c))) --> selectedCell))
        mods ++ observer.map { o =>
          Seq(onClick.mapTo(CellClicked(r, key, c)) --> o)
        }.getOrElse(Seq())

      case ActionColumn(_, l, renderer) =>
        val mods = renderer.render(l)
        val omods: Seq[Modifier[ReactiveHtmlElement[dom.html.Element]]] = observer.map { o =>
          Seq(
            onClick.mapTo(Some(CellSelected(r, key, c))) --> selectedCell,
            onClick.mapTo(CellClicked(r, key, c)) --> o,
            onClick.mapTo(Action(r, key, c)) --> o
          )
        }.getOrElse(Seq())
        Seq(
          button(
            mods ++ omods
          ),
          onClick.mapTo(Some(CellSelected(r, key, c))) --> selectedCell,
        )

    }

  def treeTable[R, K](keyFn: R => K, data: Signal[Seq[TreeNode[R]]], tmods: TableConfigModifier[R, K] *)/*(implicit ord: Ordering[K])*/ = {
    implicit val observer: Option[Observer[TableEvent[R, K]]] = None
    val tblConfig = tmods.foldLeft(TableConfig[R, K](keyFn, Seq())) {
      case (tc, tm) => tm.modify(tc)
    }

    val empty: Map[K, Item[R, K]] = Map()
    val state: Var[IS[R, K]] = Var(IS(empty))
    //val selectedRow: Var[Option[K]] = Var(None)
    val selectedCell: Var[Option[CellSelected[R, K]]] = Var(None)
    implicit val selectedSellObs = selectedCell.writer
    val ord: Ordering[R] = tblConfig.columns.collect{ case ord: TableColumn[_, _] => ord}.headOption.getOrElse(noordering)
    val flatData = state.signal.map(is => is.flatten(ord))


    def renderRow(key: K, tn: Item[R, K], signal: Signal[Item[R, K]]): ReactiveHtmlElement[html.TableRow] = {

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
        val cellMods = tc.map(c => renderCell( key, r, c)).getOrElse(Seq())
        td(
          mods ++ cellMods
        )
      }

      tr(

        cls <-- selectedCell.signal.map(_.map(sr => if (sr.key == tn.key) Styles.selectedRow.htmlClass else "").getOrElse("")),
        child <-- signal.map { r =>
          renderHeadCell(r.row, tblConfig.columns.headOption)
        },

        children <-- signal.map { r =>
          tblConfig.columns.tail.map { c =>
            td(renderCell( key, r.row, c))
          }
        }

      )
    }

    table(
      tblConfig.selectedRow.map(sr => selectedCell.signal.map(x =>  x.map(_.row)) --> sr),
      observer.map( o => selectedCell.signal.changes.collect{case Some(evt) => evt} --> o) ,
      cls := Seq(Styles.tableBordered.htmlClass, Styles.table.htmlClass),
      data.map(nList => state.now().merge(nList, tblConfig.key)) --> Observer(x => state.set(x)),
      thead(
        tr(
          tblConfig.columns.map { c =>
            th(c.name)
          }
        )
      ),
      tbody(
        children <-- flatData.split(i => i.key)(renderRow)
      )
    )
  }


  def renderTable[R, K](keyFn: R => K, data: Signal[Seq[R]], tmods: TableConfigModifier[R, K] *) = {
    implicit val observer: Option[Observer[TableEvent[R, K]]] = None

    val selectedCell: Var[Option[CellSelected[R, K]]] = Var(None)
    implicit val selectedSellObs = selectedCell.writer

    val tblConf = tmods.foldLeft(TableConfig[R, K](keyFn, Seq())) {
      case (tc, tm) => tm.modify(tc)
    }

    def renderRow(key: K, row: R, signal: Signal[R]) = {

      tr(
        cls <-- selectedCell.signal.map(_.map(sr => if (sr.key == key) Styles.selectedRow.htmlClass else "").getOrElse("")),
        children <-- signal.map { r =>
          tblConf.columns.map { c =>
            td(renderCell(key, r, c))
          }
        }
      )
    }

    table(
      cls := Seq(Styles.tableBordered.htmlClass, Styles.table.htmlClass),
      tblConf.selectedRow.map(sr => selectedCell.signal.map(x =>  x.map(_.row)) --> sr),
      observer.map( o => selectedCell.signal.changes.collect{case Some(evt) => evt} --> o) ,

      thead(
        tr(
          tblConf.columns.map { c =>
            th(c.name)
          }
        )
      ),
      tbody(
        children <-- data.split(x => tblConf.key(x))(renderRow)
      )
    )
  }
  
}
