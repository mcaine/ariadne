package com.mikeycaine.ariadne

case class Dijkstra(maze: GridMaze) {
  def distances(from: GridMazeCell): Map[GridMazeCell, Int] = {
    var d = Map[GridMazeCell, Int]()
    d += (from -> 0)
    
    var frontier = List[GridMazeCell](from)

    while (frontier.nonEmpty) {

      var newFrontier = List[GridMazeCell]()

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
