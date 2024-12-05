package com.mikeycaine.ariadne

object HunterKillerAlgorithm {

  import Utils.*

  def randomCell(maze: Maze[_, Cell]): Cell = sample(maze.allCells)
  def unvisitedNeighbours(cell: Cell): List[Cell] = cell.neighbours.filter(_.canGoTo.isEmpty)
  def visitedNeighbours(cell: Cell): List[Cell] = cell.neighbours.filter(_.canGoTo.nonEmpty)
  def unlinkedCells(maze: Maze[_, Cell]) = maze.allCells.filter(_.canGoTo.isEmpty)

  private def linkIt(maze: Maze[_, Cell]): Unit = {

    @annotation.tailrec
    def process(current: Cell): Unit = {
      val unvisited = unvisitedNeighbours(current)
      if (unvisited.nonEmpty) {
        val neighbour = sample(unvisited)
        current.link(neighbour)
        process(neighbour)
      } else {
        val unlinked = unlinkedCells(maze)
        val candidates = unlinked.filter(visitedNeighbours(_).nonEmpty)
        if (candidates.nonEmpty) {
          val next = sample(candidates)
          val neighbour = sample(visitedNeighbours(next))
          next.link(neighbour)
          process(next)
        }
      }
    }

    process(randomCell(maze))
  }

  def apply(rows: Int, cols: Int): GridMaze = {
    val maze = GridMaze(rows, cols)
    linkIt(maze)
    maze
  }
}
