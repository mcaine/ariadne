package com.mikeycaine.ariadne

object HunterKillerAlgorithm {

  import Utils.*

  def randomCell(maze: Maze[_, GridMazeCell]): GridMazeCell = sample(maze.allCells)
  
  def unvisitedNeighbours(maze: Maze[_,GridMazeCell], cell: GridMazeCell): List[GridMazeCell] = maze.neighbours(cell).filter(_.canGoTo.isEmpty)
  def visitedNeighbours(maze: Maze[_,GridMazeCell], cell: GridMazeCell): List[GridMazeCell] = maze.neighbours(cell).filter(_.canGoTo.nonEmpty)
  def unlinkedCells(maze: Maze[_, GridMazeCell]) = maze.allCells.filter(_.canGoTo.isEmpty)

  @annotation.tailrec
  def process(mz: Maze[_, GridMazeCell], current: GridMazeCell): Unit = {
    val unvisited = unvisitedNeighbours(mz, current)
    if (unvisited.nonEmpty) {
      val neighbour = sample(unvisited)
      current.link(neighbour)
      process(mz, neighbour)
    } else {
      val unlinked = unlinkedCells(mz)
      val candidates = unlinked.filter(visitedNeighbours(mz, _).nonEmpty)
      if (candidates.nonEmpty) {
        val next = sample(candidates)
        val neighbour = sample(visitedNeighbours(mz, next))
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
