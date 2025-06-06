package com.mikeycaine.ariadne

case class Dijkstra(maze: GridMaze) {
  def distances(from: Cell): Map[Cell, Int] = {
    var d = Map[Cell, Int]()
    d += (from -> 0)
    
    var frontier = List[Cell](from)

    while (frontier.nonEmpty) {

      var newFrontier = List[Cell]()

      frontier.foreach { cell =>
        val newDist = d(cell) + 1
        cell.canGoTo.foreach { linkedCell =>
          if (!d.keySet.contains(linkedCell.contents)) {
            d += (linkedCell.contents -> newDist)
            newFrontier = linkedCell.contents :: newFrontier
          }
        }
      }

      frontier = newFrontier
    }

    d
  }
}
