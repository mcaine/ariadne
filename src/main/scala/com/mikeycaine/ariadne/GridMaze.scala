package com.mikeycaine.ariadne

import java.awt.Color

trait Maze[K,C] {
  private var cells: Map[K,C] = initCells
  def initCells: Map[K,C]

  def at(k: K): Option[C] = cells.get(k)
  def allCells: List[C] = cells.values.toList
}

case class Cell(row: Int, col: Int, grid: GridMaze) { me: Cell =>

  private var links: Set[Cell] = Set.empty[Cell]

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

  def isLinkedTo(target: Cell): Boolean = links.contains(target)
  def isLinkedTo(targetOpt: Option[Cell]): Boolean = targetOpt match {
    case Some(cell) => isLinkedTo(cell)
    case _ => false
  }

  def north: Option[Cell] = grid.at(row - 1, col)
  def south: Option[Cell] = grid.at(row + 1, col)
  def west: Option[Cell] = grid.at(row, col - 1)
  def east: Option[Cell] = grid.at(row, col + 1)

  def canGoNorth: Boolean = isLinkedTo(north)
  def canGoSouth: Boolean = isLinkedTo(south)
  def canGoWest: Boolean = isLinkedTo(west)
  def canGoEast: Boolean = isLinkedTo(east)

  def neighbours: List[Cell] = north.toList ++ south.toList ++ east.toList ++ west.toList

  def canGoTo: List[Cell] = links.toList
}

case class GridMaze(rows: Int, cols: Int ) extends Maze[(Int,Int), Cell] { me =>
  override def initCells: Map[(Int, Int), Cell] = (for {
    row <- 0 until rows
    col <- 0 until cols
  } yield (row, col) -> Cell(row, col, me)).toMap
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
