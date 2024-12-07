package com.mikeycaine.ariadne

case class Dijkstra(maze: GridMaze) {

  def distances(fromRow: Int, fromCol: Int): Map[Cell, Int] = {
    var d = Map[Cell, Int]()

    maze.at(fromRow, fromCol) foreach { from =>

      d += (from -> 0)
      var frontier = List[Cell](from)

      while (frontier.nonEmpty) {

        var newFrontier = List[Cell]()

        frontier.foreach { cell =>
          val newDist = d(cell) + 1
          cell.canGoTo.foreach { linkedCell =>
            if (!d.keySet.contains(linkedCell)) {
              d += (linkedCell -> newDist)
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
