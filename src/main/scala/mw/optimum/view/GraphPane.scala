package mw.optimum.view

import scalafx.scene.layout.Pane

class GraphPane extends Pane {
  var currentGraph = Graph.empty
  def show(newGraph: Graph): Unit = {
    children = newGraph.links ++ newGraph.bubbles
    currentGraph = newGraph
  }
}
