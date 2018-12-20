import Optimum.company
import mw.optimum.model.Company
import mw.optimum.view.{Graph, GraphPane}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ScrollPane, TextField}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.FileChooser

import scala.util.Try

object Optimum extends JFXApp {
  var undoStack = List.empty[Company]
  var company = Company.empty
  var redoStack = List.empty[Company]
  val graphPane = new GraphPane
  stage = new PrimaryStage {
    title = "Optimum"
    scene = new Scene {
      root = new BorderPane {
        top = new HBox {
          val undoButton = new Button {
            text = "Undo"
            disable = true
            onAction = { _ =>
              redoStack ::= company
              company = undoStack.head
              undoStack = undoStack.tail
              refresh()
            }
          }
          val redoButton = new Button {
            text = "Redo"
            disable = true
            onAction = { _ =>
              undoStack ::= company
              company = redoStack.head
              redoStack = redoStack.tail
              refresh()
            }
          }
          val nextButton = new Button {
            text = "Next"
            disable = true
          }
          val sizeField = new TextField {
            text = "7"
          }
          val loadButton = new Button("Load") {
            onAction = { _ =>
              val chooser = new FileChooser
              chooser.showOpenDialog(stage) match {
                case null =>
                case file =>
                  undoStack = Nil
                  company = Company.loadFrom(file)
                  redoStack = company.start :: Nil
                  refresh()
              }
            }
          }
          children = loadButton :: undoButton :: redoButton :: nextButton :: sizeField :: Nil
          def refresh(): Unit = {
            val maxTribeSize = Try(sizeField.text().toInt).getOrElse(0)
            val previousGraph = graphPane.currentGraph
            val newGraph = Graph(company, maxTribeSize, previousGraph)
            graphPane.show(newGraph)
            undoButton.disable = undoStack.isEmpty
            redoButton.disable = redoStack.isEmpty
            nextButton.disable = newGraph.mostCoupled.isEmpty
          }
        }
        center = new ScrollPane {
          content = graphPane
        }
      }
    }
  }
}
