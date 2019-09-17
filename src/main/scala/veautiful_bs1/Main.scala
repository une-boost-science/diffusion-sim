package veautiful_bs1

import com.wbillingsley.veautiful.{<, Attacher, DElement, ElementComponent, Update, VNode, ^}
import org.scalajs.dom
import org.scalajs.dom.Node
import org.scalajs.dom.raw.HTMLInputElement

import scala.collection.mutable.ArrayBuffer
import org.scalajs.dom.svg.Circle

import scala.scalajs.js.timers
import scala.scalajs.js.timers.SetIntervalHandle
import scala.util.{Random, Try}

object Main {


  /** A molecule is a point that has a class so we can see purple ones and green ones */
  case class Molecule(var position: Vec2, ordinary:Boolean = true) {

    /** Moves in a random direction within a container */
    def move(h:Double, within:Rect):Unit = {
      val p2 = within.limit(position + Vec2.randomDir(0.225 * h))
      position = p2
    }
  }

  case class Ring(p:Vec2, r:Double) {
    def randomPoint():Vec2 = {
      Vec2.randomDir(Math.random() * r) + p
    }

    def contains(p2:Vec2):Boolean = {
      (p2 - p).magnitude < r
    }
  }

  case class Rect(p1:Vec2, p2:Vec2) {
    def limit(p:Vec2): Vec2 = {
      Vec2(
        Math.max(p1.x, Math.min(p.x, p2.x)),
        Math.max(p1.y, Math.min(p.y, p2.y))
      )
    }

    def randomPoint():Vec2 = {
      Vec2(
        p1.x + Math.random() * (p2.x - p1.x),
        p1.y + Math.random() * (p2.y - p1.y)
      )
    }
  }

  object Simulation {

    var tick = 0;

    var results:ArrayBuffer[(Int, Int)] = ArrayBuffer.empty

    /** The container molecules can't escape */
    val bounds = Rect(Vec2(0,0), Vec2(640, 480))

    /** The boundary ring for the experiment */
    val boundaryRing = Ring(Vec2(320, 240), 50)

    /** The molecules we measure the diffusion of start within this ring */
    val dropRing = Ring(Vec2(320, 240), 20)

    val molecules:ArrayBuffer[Molecule] = ArrayBuffer.empty

    var numOrdinary = 1000//00

    var numTracked = 200

    var heat:Double = 5

    def outsideBoundary = {
      molecules.filterNot({ m =>
        m.ordinary || boundaryRing.contains(m.position)
      }).size
    }

    def reset():Unit = {
      tick = 0
      molecules.clear()

      val ord = for { _ <- 0 until numOrdinary } yield Molecule(bounds.randomPoint())
      val tracked = for { _ <- 0 until numTracked } yield Molecule(dropRing.randomPoint(), false)

      molecules.append((ord ++ tracked):_*)
    }

    def step():Unit = {
      for { m <- molecules } m.move(heat, bounds)
      tick += 1

      if (tick == 500) {
        results.append(heat.toInt -> outsideBoundary)
      }
    }

    reset()

  }

  class MoleculeView(m:Molecule) extends VNode with Update {

    var domNode: Option[Circle] = None

    override def attach(): Node = {
      val c = <.circle(
        ^.attr("cx") := m.position.x, ^.attr("cy") := m.position.y, ^.attr("r") := 3,
        ^.cls := (if (m.ordinary) "molecule ordinary" else "molecule tracked")
      ).create().asInstanceOf[Circle]

      domNode = Some(c)
      c
    }

    override def detach(): Unit = {
      domNode = None
    }

    def update(): Unit = {
      for { c <- domNode } {
        c.setAttribute("cx", m.position.x.toString)
        c.setAttribute("cy", m.position.y.toString)
      }
    }

  }

  case object SimulationView extends ElementComponent(<.div()) {
    // And these variables are used to keep track of how long it took us to render ourselves

    var moleculeNodes = Simulation.molecules.map { m => new MoleculeView(m) }

    def resetMoleculeViews() {
      moleculeNodes = Simulation.molecules.map { m => new MoleculeView(m) }
    }

    /**
     * The SVG that will contain the asteroid field
     */
    def svg:DElement = <.svg.attrs(
      ^.attr("viewbox") := s"${Simulation.bounds.p1.x} ${Simulation.bounds.p1.y} ${Simulation.bounds.p2.x} ${Simulation.bounds.p2.y}",
      ^.attr("width") := s"${Simulation.bounds.p2.x - Simulation.bounds.p1.x}",
      ^.attr("height") := s"${Simulation.bounds.p2.y - Simulation.bounds.p1.y}"
    )

    /** Turns an asteroid into an SVG DElement */
    def svgMolecule(m:Molecule):VNode = {
      <("circle", ns=DElement.svgNS, u=Random.nextString(7))(
        ^.attr("cx") := m.position.x, ^.attr("cy") := m.position.y, ^.attr("r") := 3,
        ^.cls := (if (m.ordinary) "molecule ordinary" else "molecule tracked")
      )
    }

    /** Turns an asteroid into an SVG DElement */
    def svgRing(m:Ring):VNode = {
      <.circle(^.attr("cx") := m.p.x, ^.attr("cy") := m.p.y, ^.attr("r") := m.r, ^.cls := "ring")
    }

    def table():VNode = <.div(^.cls := "results",
      <.table(^.cls := "table table-bordered results-table",
        <.tbody(
          <.tr(
            <.th(^.attr("scope") := "row", "Trial"),
            Simulation.results.indices.map { i => <.th(s"${i+1}")}
          ),
          <.tr(
            <.th(^.attr("scope") := "row", "Heat"),
            Simulation.results.map({ case (heat, _) => <.td(heat.toString) })
          ),
          <.tr(
            <.th(^.attr("scope") := "row", "Outside"),
            Simulation.results.map({ case (_, num) => <.td(num.toString) })
          )
        )
      )
    )

    /** A function to work out what the local VDOM should look like for the current asteroids */
    def sim():VNode = {
        svg(
          svgRing(Simulation.boundaryRing),
          moleculeNodes
        )
    }

    def simFooter():VNode = {
      <.p(s"Tick: ${Simulation.tick}  Outside boundary: ${Simulation.outsideBoundary}")
    }

    def scatterPlot():VNode = {

      val plotWidth = 540
      val plotHeight = 380

      def tickInterval(max:Int, num:Int):Int = {
        Math.ceil(max.toDouble / num).toInt
      }

      val xMax = if (Simulation.results.isEmpty) 20 else {
        Math.max(20, Simulation.results.maxBy(_._1)._1)
      }
      val yMax = if (Simulation.results.isEmpty) 100 else {
        Math.max(100, Simulation.results.maxBy(_._2)._2)
      }


      val xInterval = tickInterval(xMax, 10)
      def xScale(v:Int):Int = {
        val adj = 10 * xInterval
        val ratio = plotWidth.toDouble / adj
        (ratio * v).toInt
      }

      val yInterval = tickInterval(yMax, 5)
      def yScale(v:Int):Int = {
        val adj = 5 * yInterval
        val ratio = plotHeight.toDouble / adj
        plotHeight - (ratio * v).toInt
      }

      def xAxis(ticks:Int):VNode = {
        <("g", ns=DElement.svgNS)(
          <("line", ns=DElement.svgNS)(^.attr("x1") := "0", ^.attr("x2") := plotWidth.toString, ^.attr("y1") := plotHeight.toString, ^.attr("y2") := plotHeight.toString),
          <("text", ns=DElement.svgNS)(
            ^.attr("x") := plotWidth.toString, ^.attr("y") := "430", ^.cls := "axis-label-x", "Heat setting"
          ),
          for {
            i <- 0 to ticks
          } yield {
            val v = i * xInterval
            val x = xScale(v)

            <("g", ns=DElement.svgNS)(
              <("line", ns=DElement.svgNS)(
                ^.attr("x1") := x.toString, ^.attr("x2") := x.toString, ^.attr("y1") := plotHeight.toString, ^.attr("y2") := (plotHeight + 10).toString, ^.cls := "tick-line"
              ),
              <("text", ns=DElement.svgNS)(
                ^.attr("y") := (plotHeight + 30).toString, ^.attr("x") := x.toString, ^.cls := "tick-label-x",
                v.toString
              )
            )
          }
        )
      }

      def yAxis(ticks:Int):VNode = {

        <("g", ns=DElement.svgNS)(
          <("line", ns=DElement.svgNS)(
            ^.attr("x1") := "0", ^.attr("x2") := "0", ^.attr("y1") := "0", ^.attr("y2") := plotHeight.toString
          ),
          <("text", ns=DElement.svgNS)(
            ^.attr("x") := "0", ^.attr("y") := "0", ^.cls := "axis-label-y", "Outside at tick=500"
          ),
          for {
            i <- 0 to ticks
          } yield {
            val v = i * yInterval
            val y = yScale(v)

            <("g", ns=DElement.svgNS)(
              <("line", ns=DElement.svgNS)(
                ^.attr("x1") := "0", ^.attr("x2") := "-10", ^.attr("y1") := y.toString, ^.attr("y2") := y.toString, ^.cls := "tick-line"
              ),
              <("text", ns=DElement.svgNS)(^.attr("y") := y.toString, ^.attr("x") := "-20", ^.cls := "tick-label-y",
                v.toString
              )
            )
          }
        )
      }

      def plotPoint(heat:Int, outside:Int) = {
        val cx = xScale(heat)
        val cy = yScale(outside)

        <.circle(
          ^.attr("cx") := cx.toString, ^.attr("cy") := cy.toString, ^.attr("r") := "3", ^.cls := "plot-point"
        )
      }

      <.svg.attrs(
        ^.attr("viewBox") := "-60 -20 620 460",
        ^.attr("width") := "640",
        ^.attr("height") := "480"
      )(

        xAxis(10),
        yAxis(5),
        for { (heat, outside) <- Simulation.results } yield plotPoint(heat, outside)
      )
    }

    def simControls():VNode = {
      <.div(^.cls := "form-row",
        <.div(^.cls := "input-group col-md-5",
          <.div(^.cls := "input-group-prepend", <.span(^.cls := "input-group-text", "Heat")),
          <("input")(^.attr("type") := "number", ^.cls := "form-control",
            ^.prop("value") := Simulation.heat.toString,
            if (Simulation.tick > 0) {
              ^.attr("disabled") := "true"
            } else ^.attr("enabled") := "true",
            ^.on("input") ==> { event =>
              event.target match {
                case i: HTMLInputElement =>
                  Simulation.heat = Math.max(0, try {
                    i.value.toInt
                  } catch {
                    case _: Throwable => 0
                  })
                  rerender()
                case _ => // do nothing
              }
            }
          ),
          if (ticking) {
            <.div(^.cls := "input-group-append",
              <("button", "play")(^.attr("id") := "pause",
                ^.cls := "btn btn-sm btn-secondary", ^.onClick --> stopTicking(),
                <("i")(^.cls := "fa fa-pause")
              ),
              <("button")(
                ^.cls := "btn btn-sm btn-secondary", ^.attr("disabled") := "true", ^.onClick --> reset, "Reset"
              )
            )
          } else {
            <.div(^.cls := "input-group-append",
              <("button", "pause")(^.attr("id") := "play",
                ^.cls := "btn btn-sm btn-secondary", ^.onClick --> startTicking(),
                <("i")(^.cls := "fa fa-play")
              ),
              <("button")(
                ^.cls := "btn btn-sm btn-secondary", ^.onClick --> reset, "Reset"
              )
            )
          }
        )
      )
    }

    /** The function we're calling on every tick to re-render this bit of UI */
    def rerender():Unit = {
      // We do our rendering just by telling our component's local root node
      // (the <.div() up in the constructor) to update itself so it has the children that
      // are returned by card(asteroids). ie, we're updating a local virtual DOM.
      val r = try {
        renderElements(<.div(
          <.div(^.cls := "card mb-3",
            <.div(^.cls := "card-header",
              "Simulation"
            ),
            <.div(^.cls := "card-image",
              sim()
            ),
            <.div(^.cls := "card-footer",
              simControls(),
              simFooter()
            )
          ),
          <.div(^.cls := "card mb-3",
            <.div(^.cls := "card-header",
              "Results"
            ),
            <.div(^.cls := "card-body",
              """
                | Each time you run the simulation, the table below will record the heat value and how many molecules
                | were outside the boundary at tick = 500. Run the simulation several times with different heat values
                | to populate the table.
                |""".stripMargin,
            ),
            <.div(^.cls := "card-body",
              table()
            )
          ),
          <.div(^.cls := "card mb-3",
            <.div(^.cls := "card-header",
              "Scatter plot"
            ),
            <.div(^.cls := "card-body",
              """
                | The scatter plot below automatically plots the data you collected when you ran the simulation.
                |""".stripMargin
            ),
            <.div(^.cls := "card-img",
              scatterPlot()
            )
          )
        ))
      } catch {
        case x:Throwable =>
          renderElements(<.div("Error: " + x.getMessage))
          x.printStackTrace()
      }
    }

    var ticking = false

    def startTicking(): Unit = {
      println("Starting ticking")

      ticking = true
      def tick(t:Double):Unit = {
        Simulation.step()
        rerender()

        if (ticking) dom.window.requestAnimationFrame(tick)
      }
      dom.window.requestAnimationFrame(tick)
    }

    def stopTicking():Unit = {
      println("Stopping ticking")

      ticking = false
    }

    def reset():Unit = {
      Simulation.reset()
      resetMoleculeViews()
      rerender()
    }

    override def afterAttach(): Unit = {
      super.afterAttach()
      reset()
      rerender()
    }

  }


  def main(args:Array[String]): Unit = {
    Simulation.reset()

    val root = Attacher.newRoot(dom.document.getElementById("render-here"))
    root.render(SimulationView)
  }

}
