package com.mikeycaine.ariadne

object HunterKillerAlgorithm {

  import Utils.*

  def randomCell(maze: Maze[_, Cell]): Cell = sample(maze.allCells)
  def unvisitedNeighbours(cell: Cell): List[Cell] = cell.neighbours.filter(_.canGoTo.isEmpty)
  def visitedNeighbours(cell: Cell): List[Cell] = cell.neighbours.filter(_.canGoTo.nonEmpty)
  def unlinkedCells(maze: Maze[_, Cell]) = maze.allCells.filter(_.canGoTo.isEmpty)

  @annotation.tailrec
  def process(mz: Maze[_, Cell],  current: Cell): Unit = {
    val unvisited = unvisitedNeighbours(current)
    if (unvisited.nonEmpty) {
      val neighbour = sample(unvisited)
      current.link(neighbour)
      process(mz, neighbour)
    } else {
      val unlinked = unlinkedCells(mz)
      val candidates = unlinked.filter(visitedNeighbours(_).nonEmpty)
      if (candidates.nonEmpty) {
        val next = sample(candidates)
        val neighbour = sample(visitedNeighbours(next))
        next.link(neighbour)
        process(mz, next)
      }
    }
  }

  def apply(rows: Int, cols: Int): GridMaze = {
    val maze = GridMaze(rows, cols)
    process(maze, randomCell(maze))
    maze
  }
}
