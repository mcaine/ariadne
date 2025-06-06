package com.mikeycaine.ariadne

import scala.collection.mutable


case class RouteSearch(val grid: GridMaze) {

  def startAt(start: GridMazeCell, target: GridMazeCell): Seq[GridMazeCell] = {

    val q = mutable.Queue.empty[List[GridMazeCell]]
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
}
