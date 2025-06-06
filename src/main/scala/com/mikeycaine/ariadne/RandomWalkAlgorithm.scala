package com.mikeycaine.ariadne

import java.util.Random

object RandomWalkAlgorithm {

  private def sample[A](list: List[A]) = {
    val r = new Random()
    val i = r.nextInt(list.size)
    list(i)
  }

  private def linkIt(maze: GridMaze): Unit = {
    var visited: Set[GridMazeCell] = Set.empty

    val rand = new Random
    val startRow = rand.nextInt(maze.rows)
    val startCol = rand.nextInt(maze.cols)

    var current: GridMazeCell = maze.at(startRow, startCol).get

    while (visited.size < maze.rows * maze.cols) {
      visited = visited + current
      val neighbours = maze.neighbours(current)
      val neighbour = neighbours(rand.nextInt(neighbours.size))
      if (visited.contains(neighbour)) {
        current = neighbour
      } else {
        current.link(neighbour)
        current = neighbour
      }
    }
  }

  def apply(rows: Int, cols: Int): GridMaze = {
    val maze = GridMaze(rows, cols)
    linkIt(maze)
    maze
  }
}
