import mw.opti.OptimusPane
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ScrollPane, TextField}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.scene.paint.Color.Black
import scalafx.stage.FileChooser
import scalafx.Includes._

object Optimus extends JFXApp {
	val optimus = new OptimusPane
	stage = new PrimaryStage {
		title = "Optimus"
		scene = new Scene {
			root = new BorderPane {
				fill = Black
				top = new HBox {
					val loadButton = new Button("Load") {
						onAction = { _: ActionEvent =>
							val chooser = new FileChooser
							chooser.showOpenDialog(stage) match {
								case null =>
								case file => optimus.load(file)
							}
						}
					}
					val undoButton = new Button("Undo")
					val redoButton = new Button("Redo")
					val mergeButton = new Button("Merge")
					val sizeField = new TextField {
						text = "7"
					}
					children = loadButton :: undoButton :: redoButton :: mergeButton :: sizeField :: Nil
				}
				center = new ScrollPane {
					content = optimus
				}
			}
		}
	}
}
