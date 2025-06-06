package com.mikeycaine.ariadne

import scala.collection.mutable


case class RouteSearch(val grid: GridMaze) {
//  def walk: Seq[Cell] = {
//
//    val row = 0
//    val col = 0
//
//    // TODO handle the case the target doesnt exist
//    //val target = grid.at(grid.rows - 1, grid.cols - 1).get
//
//    grid.at(row, col) match {
//      case None => throw new Exception("No start cell") // TODO
//      case Some(cell) => startAt(cell, target)
//    }
//  }
//  
//  def walk(row: Int, col: Int) :

  def startAt(start: Cell, target: Cell): Seq[Cell] = {

    val q = mutable.Queue.empty[List[Cell]]
    var current = List(start)

    while {
      val end = current.head
      val choices = end.canGoTo.filter(!current.contains(_))

      choices.foreach { choice =>
        q.enqueue(choice.contents +: current)
      }

      current = q.dequeue()

      !current.contains(target)
    } do ()

    current.reverse
  }

  //  // TODO handle empty list
  //  def oneOf[F](from: Seq[F]) = {
  //    val r = new Random
  //    val i = r.nextInt(from.size)
  //    from(i)
  //  }
}
