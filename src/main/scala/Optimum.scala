import mw.optimum.model.Company
import mw.optimum.view.GraphPane
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ScrollPane}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.FileChooser

object Optimum extends JFXApp {
  var undoStack = List.empty[Company]
  var redoStack = List.empty[Company]
  val graph = new GraphPane
  stage = new PrimaryStage {
    title = "Optimum"
    scene = new Scene {
      root = new BorderPane {
        top = new HBox {
          val loadButton = new Button("Load") {
            onAction = { _ =>
              val chooser = new FileChooser
              chooser.showOpenDialog(stage) match {
                case null =>
                case file =>
                  graph.company = Company.loadFrom(file)
              }

            }
          }
          val undoButton = new Button("Undo")
          val redoButton = new Button("Redo")
          val nextButton = new Button("Next")
          children = loadButton :: undoButton :: redoButton :: nextButton :: Nil
        }
        center = new ScrollPane {
          content = graph
        }
      }
    }
  }
}
