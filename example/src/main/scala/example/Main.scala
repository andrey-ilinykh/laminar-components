package example

import com.raquo.airstream.core.EventStream
import com.raquo.airstream.ownership.ManualOwner
import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L._
import lc.view._
import lc.model.{Column, TableEvent, TreeNode}
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


  implicit val owner = new ManualOwner
  timer.foreach { _ =>
    persons = persons.map(f => f.copy(age = f.age + 1, citizen = !f.citizen))
    data.set(persons)
  }

  def renderPage() = {

    div(
      Table.treeTable[Person, String](
        TableConfig[Person, String](
          _.name,
        ).withColumn("Name", _.name)
          .withColumn("Age", _.age)
          .withColumn("Double Age", _.age * 2)
          .withColumn("Address", _.address)
          .withColumn("Citizen", _.citizen)
          .action("Action", "x"),
        nodeData,
        Some(Observer[TableEvent[Person, String]]{
          case evt => println(evt)
        })
      ),
      p(),
      Table.renderTable1[Person, String](
        Seq(
          Column[Person]("Name", _.name),
          Column[Person]("Age", _.age.toString),
          Column[Person]("Address", _.address),
        ),
        _.name,
        data.signal
      ),
      div("Simple table"),
      Table.renderTable(TableConfig[Person, String](
        _.name,
      ).withColumn("Name", _.name)
       .withColumn("Age", _.age)
        .withColumn("Double Age", _.age * 2)
       .withColumn("Address", _.address)
       .withColumn("Citizen", _.citizen)
        .action("Action", "x")
        , data.signal,
          Some(Observer[TableEvent[Person, String]]{
          case evt => println(evt)
        }
        )
      )
    )
  }

  def main(args: Array[String]): Unit = {
    lazy val container = dom.document.getElementById("app-container")
    //renderOnDomContentLoaded(container, renderPage(Commands.commandObserver, Data.errorSignal.signal))
    renderOnDomContentLoaded(container, renderPage())
  }
}
