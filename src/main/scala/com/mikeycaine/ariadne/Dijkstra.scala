package com.mikeycaine.ariadne

case class Dijkstra(val maze: GridMaze) {

  def distances(fromRow: Int, fromCol: Int): Map[Cell, Int] = {
    var d = Map[Cell, Int]()

    maze.at(fromRow, fromCol) foreach { from =>

      d += (from -> 0)
      var frontier = List[Cell](from)

      while (!frontier.isEmpty) {

        var newFrontier = List[Cell]()

        frontier.foreach { cell =>
          cell.canGoTo.foreach { linkedCell =>
            if (!d.keySet.contains(linkedCell)) {
              d += (linkedCell -> (d(cell) + 1))
              newFrontier = linkedCell :: newFrontier
            }
          }
        }

        frontier = newFrontier
      }
    }

    d
  }
}
