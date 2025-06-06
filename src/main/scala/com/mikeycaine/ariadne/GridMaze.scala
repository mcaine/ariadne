package com.mikeycaine.ariadne

import java.awt.Color

trait Maze[K,C] {
  private var cells: Map[K,C] = initCells
  def initCells: Map[K,C]

  def at(k: K): Option[C] = cells.get(k)
  def allCells: List[C] = cells.values.toList
  def neighbours(cell: C): List[C]
}

class Cell[C <: Cell[C]] {

  var links: Set[C] = Set.empty[C]

  def link(target: C, biDirectional: Boolean = true): Unit = {
    links = links + target
    if (biDirectional) {
      target.link(this.asInstanceOf[C], false)
    }
  }

  def unlink(target: C, biDirectional: Boolean = true): Unit = {
    links = links - target
    if (biDirectional) {
      target.unlink(this.asInstanceOf[C], false)
    }
  }

  def isLinkedTo(target: C): Boolean = links.contains(target)
  def isLinkedTo(targetOpt: Option[C]): Boolean = targetOpt.map(isLinkedTo(_)).getOrElse(false)

  def canGoTo: List[Cell[C]] = links.toList

  def contents = this.asInstanceOf[C]
}

class GridMazeCell(val row: Int, val col: Int) extends Cell[GridMazeCell]

object GridMazeCell {
  def apply(row: Int, col: Int) = new GridMazeCell(row, col)
}

case class GridMaze(rows: Int, cols: Int ) extends Maze[(Int,Int), GridMazeCell] { me =>
  override def initCells: Map[(Int, Int), GridMazeCell] = (for {
    row <- 0 until rows
    col <- 0 until cols
  } yield (row, col) -> GridMazeCell(row, col)).toMap

  def northFrom(cell: GridMazeCell): Option[GridMazeCell] = at(cell.row - 1, cell.col)
  def southFrom(cell: GridMazeCell): Option[GridMazeCell] = at(cell.row + 1, cell.col)
  def westFrom(cell: GridMazeCell): Option[GridMazeCell] = at(cell.row, cell.col - 1)
  def eastFrom(cell: GridMazeCell): Option[GridMazeCell] = at(cell.row, cell.col + 1)

  def canGoNorthFrom(cell: GridMazeCell): Boolean = cell.isLinkedTo(northFrom(cell))
  def canGoSouthFrom(cell: GridMazeCell): Boolean = cell.isLinkedTo(southFrom(cell))
  def canGoWestFrom(cell: GridMazeCell): Boolean = cell.isLinkedTo(westFrom(cell))
  def canGoEastFrom(cell: GridMazeCell): Boolean = cell.isLinkedTo(eastFrom(cell))

  def neighbours(cell:GridMazeCell): List[GridMazeCell] = northFrom(cell).toList ++
                                          southFrom(cell).toList ++
                                          eastFrom(cell).toList ++
                                          westFrom(cell).toList
}

object GridMaze {
  def distanceColours(d: Map[GridMazeCell, Int]): Map[(Int, Int), Color] = {
    val maxDist = d.values.max
    val colours: Map[(Int, Int), Color] = d map {
      case (cell: GridMazeCell, distance) =>
        val c = Math.max(0, Math.min(255, 255 - 255 * distance / (Math.max(1,maxDist))))
        (cell.row, cell.col) -> new Color(c, c, 120)
    }
    colours.toMap
  }
}
