package com.mikeycaine.ariadne

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.io.File
import scala.util.{Failure, Success, Try}

class HunterKillerAlgorithmSpec extends AnyFlatSpec with Matchers {

  def checkTry[F](t: Try[F]): Unit = t match {
    case Success(_) =>
    case Failure(exception: Exception) => fail(exception.getMessage)
  }

  "HunterKillerAlgorithm" should "return a maze" in {
    val maze: GridMaze = HunterKillerAlgorithm(100, 100)

    val outputFile = new File("hunterkiller.png")
    val dijkstra = Dijkstra(maze)
    
    val d = dijkstra.distances(0, 0)
    val maxDist = d.values.max
    val colours = d map {
      case (cell: Cell, distance) =>
        val c = Math.max(0, Math.min(255, 255 - 255 * distance / maxDist))
        val d = 255 - c
        val colour = new Color(c, 120, 120)

        (cell.row, cell.col) -> colour
    }

    checkTry(Grid2Png.writeWithColours(maze, outputFile, colours))

  }

  it should "have a solution " in {

    val maze: GridMaze = HunterKillerAlgorithm(100, 100)

    //val outputFile = new File("hunterkiller2.png")
    val dijkstra = Dijkstra(maze)

    val d: Map[Cell, Int] = dijkstra.distances(0, 0)
    val dijkstraColours = GridMaze.distanceColours(d)

    val start = maze.at(0,0).get
    val end = maze.at(99,99).get

    val path = RouteSearch(maze).startAt(start, end)
    val routeColours = path.map(cell => ((cell.row, cell.col) -> Color.RED)).toMap

    val allColours = dijkstraColours ++ routeColours

    checkTry(Grid2Png.writeWithColours(maze, new File("hunterkiller2.png"), allColours))
  }

  it should "have a solution using the opposite diagonal" in {

    val maze: GridMaze = HunterKillerAlgorithm(100, 100)

    //val outputFile = new File("hunterkiller2.png")
    val dijkstra = Dijkstra(maze)

    val d: Map[Cell, Int] = dijkstra.distances(0,99)
    val dijkstraColours = GridMaze.distanceColours(d)

    val start = maze.at(0, 99).get
    val end = maze.at(99, 0).get
    val path = RouteSearch(maze).startAt(start, end)
    val routeColours = path.map(cell => ((cell.row, cell.col) -> Color.RED)).toMap

    val allColours = dijkstraColours ++ routeColours

    checkTry(Grid2Png.writeWithColours(maze, new File("hunterkiller3.png"), allColours))
  }

}
