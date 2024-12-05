package com.mikeycaine.ariadne

import java.util.Random

object HunterKillerAlgorithm {

  import Utils._

  def randomCell(maze: Maze[_, Cell]): Cell = sample(maze.allCells)

  private def linkIt(maze: Maze[_, Cell]): Unit = {

    val start = randomCell(maze)

    var current: Option[Cell] = Some(start)

    while (current.isDefined) {
      current match {
        case Some(currentCell) => {
          val unvisitedNeighbours
            = currentCell.neighbours.filter(cell => cell.canGoTo.isEmpty)

          if (unvisitedNeighbours.nonEmpty) {
            val neighbour = sample(unvisitedNeighbours)
            currentCell.link(neighbour)
            current = Some(neighbour)
          } else {
            current = None
            for (cell <- maze.allCells) {
              if (current.isEmpty && cell.canGoTo.isEmpty) {
                val visitedNeighbours = cell.neighbours.filter(_.canGoTo.nonEmpty)
                if (visitedNeighbours.nonEmpty) {
                  val neighbour = sample(visitedNeighbours)
                  cell.link(neighbour)
                  current = Some(cell)
                }
              }
            }
          }
        }
        case _ =>
      }
    }
  }

  def apply(rows: Int, cols: Int): GridMaze = {
    val maze = GridMaze(rows, cols)
    linkIt(maze)
    maze
  }
}
