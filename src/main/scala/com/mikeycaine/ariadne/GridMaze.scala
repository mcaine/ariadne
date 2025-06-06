package com.mikeycaine.ariadne

import java.awt.Color

trait Maze[K,C] {
  private var cells: Map[K,C] = initCells
  def initCells: Map[K,C]

  def at(k: K): Option[C] = cells.get(k)
  def allCells: List[C] = cells.values.toList
  def neighbours(cell: C): List[C]
}

class Linkable[C] { 

  private var links: Set[Linkable[C]] = Set.empty[Linkable[C]]

  def link(target: Linkable[C], biDirectional: Boolean = true): Unit = {
    links = links + target
    if (biDirectional) {
      target.link(this, false)
    }
  }

  def unlink(target: Linkable[C], biDirectional: Boolean = true): Unit = {
    links = links - target
    if (biDirectional) {
      target.unlink(this, false)
    }
  }

  def isLinkedTo(target: Linkable[C]): Boolean = links.contains(target)
  def isLinkedTo(targetOpt: Option[Linkable[C]]): Boolean = targetOpt match {
    case Some(l) => isLinkedTo(l)
    case _ => false
  }

  def canGoTo: List[Linkable[C]] = links.toList
  
  def contents = this.asInstanceOf[C]
}

class Cell(val row: Int, val col: Int) extends Linkable[Cell]

object Cell {
  def apply(row: Int, col: Int) = new Cell(row, col)
}

case class GridMaze(rows: Int, cols: Int ) extends Maze[(Int,Int), Cell] { me =>
  override def initCells: Map[(Int, Int), Cell] = (for {
    row <- 0 until rows
    col <- 0 until cols
  } yield (row, col) -> Cell(row, col)).toMap

  def northFrom(cell: Cell): Option[Cell] = at(cell.row - 1, cell.col)
  def southFrom(cell: Cell): Option[Cell] = at(cell.row + 1, cell.col)
  def westFrom(cell: Cell): Option[Cell] = at(cell.row, cell.col - 1)
  def eastFrom(cell: Cell): Option[Cell] = at(cell.row, cell.col + 1)

  def canGoNorthFrom(cell: Cell): Boolean = cell.isLinkedTo(northFrom(cell))
  def canGoSouthFrom(cell: Cell): Boolean = cell.isLinkedTo(southFrom(cell))
  def canGoWestFrom(cell: Cell): Boolean = cell.isLinkedTo(westFrom(cell))
  def canGoEastFrom(cell: Cell): Boolean = cell.isLinkedTo(eastFrom(cell))

  def neighbours(cell:Cell): List[Cell] = northFrom(cell).toList ++
                                          southFrom(cell).toList ++
                                          eastFrom(cell).toList ++
                                          westFrom(cell).toList
}

object GridMaze {
  def distanceColours(d: Map[Cell, Int]): Map[(Int, Int), Color] = {
    val maxDist = d.values.max
    val colours: Map[(Int, Int), Color] = d map {
      case (cell: Cell, distance) =>
        val c = Math.max(0, Math.min(255, 255 - 255 * distance / (Math.max(1,maxDist))))
        (cell.row, cell.col) -> new Color(c, c, 120)
    }
    colours.toMap
  }
}
