package com.mikeycaine.ariadne

import java.util.Random

object WilsonAlgorithm {

  private def sample[A](list: List[A]) = {
    val r = new Random()
    val i = r.nextInt(list.size)
    list(i)
  }

  private def linkIt(maze: Maze[_, Cell]): Unit = {
    var unvisited = maze.allCells
    var first = sample(unvisited)
    unvisited = unvisited.filter(_ != first)

    while (unvisited.nonEmpty) {

      var cell = sample(unvisited)
      var path = List(cell)

      while (unvisited.contains(cell)) {
        cell = sample(cell.neighbours)
        val position = path.indexOf(cell)
        if (position == -1) {
          path = path :+ cell
        } else {
          path = path.slice(0, position + 1)
        }
      }

      for (i <- 0 to path.length - 2) {
        path(i).link(path(i + 1))
        unvisited = unvisited.filter(_ != path(i))
      }
    }
  }

  def apply(rows: Int, cols: Int): GridMaze = {
    val maze = GridMaze(rows, cols)
    linkIt(maze)
    maze
  }
}
