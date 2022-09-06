package example

import com.raquo.airstream.core.EventStream
import com.raquo.airstream.ownership.ManualOwner
import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L._
import lc.view._
import lc.model.{TableEvent, TreeNode}
import org.scalajs.dom
import org.scalajs.dom.{Event, MouseEvent}


object Main {


  val timer = EventStream.periodic(50000)

  case class Person(name: String, age: Int, address: String, citizen: Boolean  = true, children: Seq[Person] = Seq())

  case class PersonNode(row: Person) extends TreeNode[Person] {

    override def children: Seq[TreeNode[Person]] = row.children.map(ch => PersonNode(ch))
  }


  var persons = Seq(
    Person("Peter", 43, "Walnut creek"),
    Person("Joe", 33, "Walnut creek", true, Seq(
      Person("First child", 12, "Walnut creek"),
      Person("Second child", 11, "Walnut creek")
    )),
    Person("Margo", 133, "Los Angeles", false)
  )
  val data = Var(persons)

  val nodeData = data.signal.map(personSeq => personSeq.map( foo =>  PersonNode(foo)))


  persons(0).equals(persons(2))
  implicit val owner = new ManualOwner
  timer.foreach { _ =>
    persons = persons.map(f => f.copy(age = f.age + 1, citizen = !f.citizen))
    data.set(persons)
  }

  def renderPage() = {

    import TableColumnModifier.sorting
    import Table._
    val events: Var[List[MouseEvent]] = Var(List())
    div(
      treeTable[Person, String](
        _.name,
        nodeData,
        column("Name", _.name, sorting.asc),
        column("Age", _.age, sorting.asc),
        column("Double Age", _.age * 2, sorting.asc),
        column("Address", _.address, sorting.asc),
        column("Citizen", _.citizen, sorting.asc),
        TableConfigModifier.selectedRow[Person, String](Observer(sr => println(s"SelectedRow = $sr")))
      ),
      p(),
      div("Simple table"),
      renderTable[Person, String](
        _.name,
        data.signal,
        column("Name", _.name, sorting.asc),
        column("Age", _.age, sorting.asc),
        column("Double Age", _.age * 2, sorting.asc),
        column("Address", _.address, sorting.asc),
        column("Citizen", _.citizen, sorting.asc),

        TableConfigModifier.selectedRow[Person, String](Observer(sr => println(s"SelectedRow = $sr")))
      ),
      canvas(
        height := "200px",
        width := "200px",
        onMouseMove --> Observer((e: MouseEvent) => events.set(e :: events.now())),
        //onPointerDown --> Observer((e: Event) => events.set(e :: events.now())),
        //onPointerMove --> Observer((e: Event) => events.set(e :: events.now())),

      ),
//      div(
//        height := "200px",
//        width := "200px",
//        onMouseDown --> Observer((e: Event) => events.set(e :: events.now())),
        //onMouseMove --> Observer((e: MouseEvent) => events.set(e :: events.now())),
//      ),
      div(
        children <-- events.signal.map(list => list.zipWithIndex.map(e => div(s"${e._1.`type`} ${e._1.clientX} ${e._1.clientY} , ${e._2}")))
      )

    )
  }

  def main(args: Array[String]): Unit = {
    lazy val container = dom.document.getElementById("app-container")
    //renderOnDomContentLoaded(container, renderPage(Commands.commandObserver, Data.errorSignal.signal))
    renderOnDomContentLoaded(container, renderPage())
  }
}
