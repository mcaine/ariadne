package com.mikeycaine.ariadne

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.io.File
import scala.util.{Failure, Success, Try}

class RandomWalkAlgorithmSpec extends AnyFlatSpec with Matchers {

  def checkTry[F](t: Try[F]): Unit = t match {
    case Success(_) =>
    case Failure(exception: Exception) => fail(exception.getMessage)
  }

  "RandomWalkAlgorithm" should "return a maze" in {
    val maze: GridMaze = RandomWalkAlgorithm(100, 100)

    val outputFile = new File("randomwalk.png")
    val dijkstra = Dijkstra(maze)

    val d = dijkstra.distances(50, 50)
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

}