import mw.optimum.model.Company
import mw.optimum.view.GraphPane
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ScrollPane}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.stage.FileChooser

object Optimum extends JFXApp {
	var company = Company.empty
	var maxTribeSize = 7
	var (nextCouple, bestScore) = company.nextCoupleAndBestScore(maxTribeSize)
	var loadScore = 0
	var undoStack = List.empty[Company]
	var redoStack = List.empty[Company]
	val graphPane = GraphPane()
	def sizeText = s" Max squads par tribe: $maxTribeSize "
	def loadScoreText = s"     Start score: $loadScore     "
	def currentScoreText = s"CurrentScore: ${company.score}     "
	def bestScoreText = s"Best score: $bestScore     "
	def dependenciesText = s"DependenciesCount: ${nextCouple.map(_._2).getOrElse(0)}     "
	stage = new PrimaryStage {
		title = "Optimum"
		scene = new Scene {
			root = new BorderPane {
				top = new HBox {
					val sizeField = new Label {
						text = sizeText
					}
					val loadScoreField = new Label {
						text = loadScoreText
					}
					val currentScoreField = new Label {
						text = currentScoreText
					}
					val bestScoreField = new Label {
						text = bestScoreText
					}
					val dependenciesCount = new Label {
						text = dependenciesText
					}
					val lessSquadsButton = new Button("-") {
						onAction = { _ =>
							maxTribeSize -= 1
							refresh()
						}
					}
					val moreSquadsButton = new Button("+") {
						onAction = { _ =>
							maxTribeSize += 1
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
									redoStack = Nil
									company = Company.loadFrom(file)
									loadScore = company.score
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
							nextCouple match {
								case Some((tribe1, _, tribe2)) =>
									undoStack ::= company
									company = company.merge(tribe1, tribe2)
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
						val (next, best) = company.nextCoupleAndBestScore(maxTribeSize)
						nextCouple = next
						bestScore = best
						sizeField.text = sizeText
						loadScoreField.text = loadScoreText
						currentScoreField.text = currentScoreText
						bestScoreField.text = bestScoreText
						dependenciesCount.text = dependenciesText
						undoButton.disable = undoStack.isEmpty
						redoButton.disable = redoStack.isEmpty
						dropButton.disable = !company.tribes.exists(_.size > 1)
						nextButton.disable = nextCouple.isEmpty
						graphPane.show(company, maxTribeSize)
					}
				}
				center = new ScrollPane {
					content = graphPane
				}
			}
		}
	}
}
