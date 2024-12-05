package com.mikeycaine.ariadne

import scala.util.Random

object AnotherAlgorithm {

  private def sample[A](list: List[A]) = {
    val r = new Random()
    val i = r.nextInt(list.size)
    list(i)
  }

  private def linkIt(maze: GridMaze): Unit = {
    val r = (0 until maze.rows by 2).toList
    val r1 = Random.shuffle(r).take(maze.rows / 2)

    val rand = new Random()
    var visited: Set[Cell] = Set.empty

    for (newRow <- r1) {
      var first = rand.nextInt(maze.cols)
      var second = rand.nextInt(maze.cols)
      if (first > second) {
        val temp = second
        second = first
        first = temp
      }

      for (c <- first until second) {
        for {
          cell <- maze.at(newRow, c)
          other <- maze.at(newRow, c + 1)
        } {
          cell.link(other)
        }
      }
    }

    val c = (0 until maze.cols by 2).toList
    val c1 = Random.shuffle(c).take(maze.cols / 2)

    for (newCol <- c1) {
      var first = rand.nextInt(maze.rows)
      var second = rand.nextInt(maze.rows)
      if (first > second) {
        val temp = second
        second = first
        first = temp
      }

      for (r <- first until second) {
        for {
          cell <- maze.at(r, newCol)
          other <- maze.at(r+1, newCol)
        } {
          cell.link(other)
        }
      }
    }

//    var unvisited = maze.allCells //.filter(_.canGoTo.isEmpty)
//
//
//    var first = sample(unvisited)
//    unvisited = unvisited.filter(_ != first)
//
//    while (unvisited.nonEmpty) {
//
//      var cell = sample(unvisited)
//      var path = List(cell)
//
//      while (unvisited.contains(cell)) {
//        cell = sample(cell.neighbours)
//        val position = path.indexOf(cell)
//        if (position == -1) {
//          path = path :+ cell
//        } else {
//          path = path.slice(0, position + 1)
//        }
//      }
//
//      for (i <- 0 to path.length - 2) {
//        path(i).link(path(i + 1))
//        unvisited = unvisited.filter(_ != path(i))
//      }
//    }
//
//
//
//
////    val startRow = rand.nextInt(maze.rows)
////    val startCol = rand.nextInt(maze.cols)
//
//    var current: Cell = first
//
//    while (visited.size < maze.rows * maze.cols) {
//      visited = visited + current
//      val neighbours = current.neighbours
//      val neighbour = neighbours(rand.nextInt(neighbours.size))
//      if (visited.contains(neighbour)) {
//        current = neighbour
//      } else {
//        current.link(neighbour)
//        current = neighbour
//      }
//    }
  }

  def apply(rows: Int, cols: Int): GridMaze = {
    val maze = GridMaze(rows, cols)
    linkIt(maze)
    maze
  }
}
