import mw.optimum.model.{Company, Squad, Tribe}
import mw.optimum.graph.Vector
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
  var scale = 1.0
  val graphPane = GraphPane(merge, move, split)
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
  val saveButton = new Button("Save") {
    onAction = { _ =>
      val chooser = new FileChooser
      chooser.showSaveDialog(stage) match {
        case null =>
        case file =>
          company.saveTo(file)
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
  val endButton = new Button {
    text = "End"
    disable = true
    onAction = { _ =>
      undoStack ::= company
      company = company.optimum(maxTribeSize)
      redoStack = Nil
      refresh()
    }
  }
  val zoomInButton = new Button {
    text = "Zoom in"
    onAction = { _ =>
      scale /= 0.75
      graphPane.scaleX = scale
      graphPane.scaleY = scale
      graphPane.translateX = graphPane.width() * (scale - 1) / 2
      graphPane.translateY = graphPane.height() * (scale - 1) / 2
    }
  }
  val zoomOutButton = new Button {
    text = "Zoom out"
    onAction = { _ =>
      scale *= 0.75
      graphPane.scaleX = scale
      graphPane.scaleY = scale
      graphPane.translateX = graphPane.width() * (scale - 1) / 2
      graphPane.translateY = graphPane.height() * (scale - 1) / 2
    }
  }
  def merge(tribe: Tribe, toTribe: Tribe) = {
    undoStack ::= company
    company = company.merge(tribe, toTribe)
    redoStack = Nil
    refresh()
  }
  def move(squad: Squad, toTribe: Tribe) = {
    undoStack ::= company
    company = company.move(squad, toTribe)
    redoStack = Nil
    refresh()
  }
  def split(squad: Squad, position: Vector) = {
    undoStack ::= company
    company = company.split(squad)
    redoStack = Nil
    val pair = company.tribes.find(_.contains(squad)).map { tribe => tribe -> position }
    refresh(pair)
  }
  def refresh(pair: Option[(Tribe, Vector)] = None): Unit = {
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
    endButton.disable = nextCouple.isEmpty
    graphPane.show(company, maxTribeSize, pair)
  }
  def name(tribe: Tribe) = if (tribe.name.startsWith("Tribe")) {
    if (tribe.size == 1) s"${tribe.squads.head.name}"
    else {
      val prefixes = tribe.squads.map(_.name.take(3)).distinct
      val sorted = prefixes.sortBy { prefix =>
        prefixes.count(_ == prefix)
      }.reverse
      s"${tribe.name} (${sorted.mkString(",")})"
    }
  } else tribe.name
  def sizeText = s" $maxTribeSize squads per tribe "
  def loadScoreText = s"     Start: $loadScore     "
  def currentScoreText = s"Current: ${company.score}     "
  def bestScoreText = s"Best: $bestScore     "
  def dependenciesText = nextCouple match {
    case Some((tribe1, weight, tribe2)) =>
      val prefix = if (weight == 1) "1 dependency" else s"$weight dependencies"
      val postfix = s"${name(tribe1)} and ${name(tribe2)}"
      s"$prefix between $postfix     "
    case None => "No dependencies     "
  }
  stage = new PrimaryStage {
    title = "Optimum"
    scene = new Scene {
      root = new BorderPane {
        top = new HBox {
          children = loadButton :: saveButton :: undoButton :: redoButton :: dropButton :: nextButton :: endButton ::
            lessSquadsButton :: sizeField :: moreSquadsButton :: zoomInButton :: zoomOutButton :: Nil
        }
        center = new ScrollPane {
          content = graphPane
        }
        bottom = new HBox {
          children = loadScoreField :: currentScoreField :: bestScoreField :: dependenciesCount :: Nil
        }
      }
    }
  }
}
