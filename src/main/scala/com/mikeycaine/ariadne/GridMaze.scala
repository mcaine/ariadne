package com.mikeycaine.ariadne

trait Maze[K,C] {
  var cells: Map[K,C] = initCells
  def initCells: Map[K,C]

  def at(k: K): Option[C] = cells.get(k)
}

case class Cell(row: Int, col: Int, grid: GridMaze) { me: Cell =>

  var links: Set[Cell] = Set[Cell]()

  def link(target: Cell, biDirectional: Boolean = true): Unit = {
    links = links + target
    if (biDirectional) {
      target.link(me, false)
    }
  }

  def unlink(target: Cell, biDirectional: Boolean = true): Unit = {
    links = links - target
    if (biDirectional) {
      target.unlink(me, false)
    }
  }

  def isLinkedTo(target: Cell) = links.contains(target)

  def north: Option[Cell] = grid.at(row - 1, col)
  def south: Option[Cell] = grid.at(row + 1, col)
  def west: Option[Cell] = grid.at(row, col - 1)
  def east: Option[Cell] = grid.at(row, col + 1)

  def canGo(cellOpt: Option[Cell]): Boolean = cellOpt map { isLinkedTo(_) } getOrElse(false)
  //
  def canGoNorth: Boolean = canGo(north)
  def canGoSouth: Boolean = canGo(south)
  def canGoWest: Boolean = canGo(west)
  def canGoEast: Boolean = canGo(east)

  def neighbours: Seq[Cell] = north.toList ++ south.toList ++ east.toList ++ west.toList

  def canGoTo: List[Cell] = links.toList
}

case class GridMaze(val rows: Int, val columns: Int ) extends Maze[(Int,Int), Cell] { me =>

  override def initCells: Map[(Int, Int), Cell] = (for {
    row <- 0 until rows
    col <- 0 until columns
  } yield (row, col) -> Cell(row, col, me)).toMap
}
