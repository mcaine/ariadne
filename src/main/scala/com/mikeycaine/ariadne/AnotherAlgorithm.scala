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
    val r1 = Random.shuffle(r).take(maze.rows / 4)

    val rand = new Random()
    var visited: Set[GridMazeCell] = Set.empty

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
    val c1 = Random.shuffle(c).take(maze.cols / 4)

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

    val cell = sample(maze.allCells.filter(_.canGoTo.nonEmpty))
    val dijkstra = Dijkstra(maze)
    val d = dijkstra.distances(cell)

    // find cells that are linked to something but not to our random cell
    val disconnected = maze.allCells.filter(_.canGoTo.nonEmpty).filter(!d.keySet.contains(_))
    for (disc <- disconnected) {
      for (dontcare <- disc.canGoTo) {
        disc.unlink(dontcare.contents)
      }
    }

    val unlinked = HunterKillerAlgorithm.unlinkedCells(maze)
    if (unlinked.nonEmpty) {
      val start = sample(unlinked)
      HunterKillerAlgorithm.process(maze, start)
    }
  }

  def apply(rows: Int, cols: Int): GridMaze = {
    val maze = GridMaze(rows, cols)
    linkIt(maze)
    maze
  }
}
