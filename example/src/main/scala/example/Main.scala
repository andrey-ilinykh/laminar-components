package example

import com.raquo.airstream.core.EventStream
import com.raquo.airstream.ownership.ManualOwner
import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L._
import lc.view._
import lc.model.{TableEvent, TreeNode}
import org.scalajs.dom


object Main {


  val timer = EventStream.periodic(50000)

  case class Person(name: String, age: Int, address: String, citizen: Boolean  = true, children: Seq[Person] = Seq())

  case class PersonNode(row: Person) extends TreeNode[Person] {

    override def children: Seq[TreeNode[Person]] = row.children.map(ch => PersonNode(ch))
  }


  var persons = Seq(
    Person("Joe", 33, "Walnut creek", true, Seq(
      Person("First child", 12, "Walnut creek"),
      Person("Second child", 11, "Walnut creek")
    )),
    Person("Peter", 43, "Walnut creek"),
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
    div(
      treeTable[Person, String](
        _.name,
        nodeData,
        column("Name", _.name, sorting.asc, sorting.desc),
        column("Age", _.age),
        column("Double Age", _.age * 2),
        column("Address", _.address),
        column("Citizen", _.citizen),
        TableConfigModifier.selectedRow[Person, String](Observer(sr => println(s"SelectedRow = $sr")))
      ),
      p(),
      div("Simple table"),
      renderTable[Person, String](
        _.name,
        data.signal,
        column("Name", _.name, sorting.asc, sorting.desc),
        column("Age", _.age),
        column("Double Age", _.age * 2),
        column("Address", _.address),
        column("Citizen", _.citizen),
        TableConfigModifier.selectedRow[Person, String](Observer(sr => println(s"SelectedRow = $sr")))
      )
    )
  }

  def main(args: Array[String]): Unit = {
    lazy val container = dom.document.getElementById("app-container")
    //renderOnDomContentLoaded(container, renderPage(Commands.commandObserver, Data.errorSignal.signal))
    renderOnDomContentLoaded(container, renderPage())
  }
}
