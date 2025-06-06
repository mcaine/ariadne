package com.mikeycaine.ariadne

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.awt.Color
import scala.util.{Failure, Success, Try}

class WilsonAlgorithmSpec extends AnyFlatSpec with Matchers {

  def checkTry[F](t: Try[F]): Unit = t match {
    case Success(_) =>
    case Failure(exception) => fail(exception.getMessage)
  }

  "WilsonAlgorithm" should "return a maze" in {
    val maze: GridMaze = WilsonAlgorithm(100, 100)

    val outputFile = new File("wilson.png")
    val dijkstra = Dijkstra(maze)
    val start = maze.at(0, 0).get

    val d = dijkstra.distances(start)
    val maxDist = d.values.max
    val colours = d map {
      case (cell: GridMazeCell, distance) =>
        val c = Math.max(0, Math.min(255, 255 - 255 * distance / maxDist))
        val d = 255 - c
        val colour = new Color(c, 120, 120)

        (cell.row, cell.col) -> colour
    }

    checkTry(Grid2Png.writeWithColours(maze, outputFile, colours))

  }

  it should "have a solution " in {

    val maze: GridMaze = WilsonAlgorithm(100, 100)

    val outputFile = new File("wilson2.png")
    val dijkstra = Dijkstra(maze)

    val start = maze.at(0, 0).get
    val end = maze.at(99, 99).get

    val d: Map[GridMazeCell, Int] = dijkstra.distances(start)
    val dijkstraColours = GridMaze.distanceColours(d)

    val path = RouteSearch(maze).startAt(start, end)
    val routeColours = path.map(cell => ((cell.row, cell.col) -> Color.RED)).toMap

    val allColours = dijkstraColours ++ routeColours

    checkTry(Grid2Png.writeWithColours(maze, outputFile, allColours))

  }

}
