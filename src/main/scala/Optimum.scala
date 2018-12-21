import mw.optimum.model.Company
import mw.optimum.view.{Graph, GraphPane}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ScrollPane}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.FileChooser

object Optimum extends JFXApp {
  var undoStack = List.empty[Company]
  var company = Company.empty
  var redoStack = List.empty[Company]
  val graphPane = new GraphPane
  var maxTribeSize = 7
  stage = new PrimaryStage {
    title = "Optimum"
    scene = new Scene {
      root = new BorderPane {
        top = new HBox {
          val sizeField = new Label {
            text = s" Max squads per tribe: $maxTribeSize "
          }
          val loadScoreField = new Label {
            text = "     Start Score: None     "
          }
          val currentScoreField = new Label {
            text = "Current score: None     "
          }
          val bestScoreField = new Label {
            text = "Best score: None     "
          }
          val dependenciesCount = new Label {
            text = "Dependencies: 0     "
          }
          val lessSquadsButton = new Button("-") {
            onAction = { _ =>
              maxTribeSize -= 1
              sizeField.text = s" Max squads per tribe: $maxTribeSize "
              refresh()
            }
          }
          val moreSquadsButton = new Button("+") {
            onAction = { _ =>
              maxTribeSize += 1
              sizeField.text = s" Max squads per tribe: $maxTribeSize "
              refresh()
            }
          }
          val loadButton = new Button("Load") {
            onAction = { _ =>
              val chooser = new FileChooser
              chooser.showOpenDialog(stage) match {
                case null =>
                case file =>
                  undoStack = Nil
                  company = Company.loadFrom(file)
                  redoStack = Nil
                  loadScoreField.text = s"     Start score: ${company.score}     "
                  refresh()
              }
            }
          }
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
          val dropButton = new Button {
            text = "Drop Tribes"
            disable = true
            onAction = { _ =>
              undoStack ::= company
              company = company.start
              redoStack = Nil
              refresh()
            }
          }
          val nextButton = new Button {
            text = "Next"
            disable = true
            onAction = { _ =>
              company.next(maxTribeSize) match {
                case Some(next) =>
                  undoStack ::= company
                  company = next
                  redoStack = Nil
                  refresh()
                case _ =>
              }
            }
          }
          children = loadButton :: undoButton :: redoButton :: dropButton :: nextButton ::
            lessSquadsButton :: sizeField :: moreSquadsButton ::
            loadScoreField :: currentScoreField :: bestScoreField :: dependenciesCount :: Nil
          def refresh(): Unit = {
            val previousGraph = graphPane.currentGraph
            val newGraph = Graph(company, maxTribeSize, previousGraph)
            graphPane.show(newGraph)
            undoButton.disable = undoStack.isEmpty
            redoButton.disable = redoStack.isEmpty
            nextButton.disable = newGraph.mostCoupled.isEmpty
            dropButton.disable = !company.tribes.exists(_.size > 1)
            currentScoreField.text = s"Current score: ${company.score}     "
            bestScoreField.text = s"Best score: ${company.bestScore(maxTribeSize)}     "
            dependenciesCount.text = s"Dependencies: ${newGraph.mostCoupled.headOption.map(_._2).getOrElse(0)}"
          }
        }
        center = new ScrollPane {
          content = graphPane
        }
      }
    }
  }
}
