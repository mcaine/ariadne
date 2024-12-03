package com.mikeycaine.ariadne

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.awt.Color
import scala.util.{Try, Success, Failure}

class WilsonAlgorithmSpec extends AnyFlatSpec with Matchers {

  def checkTry[F](t: Try[F]) = t match {
    case Success(_) =>
    case Failure(exception: Exception) => fail(exception.getMessage)
  }

  "WilsonAlgorithm" should "return a maze" in {
    val maze: GridMaze = WilsonAlgorithm(50, 150)

    val outputFile = new File("wilson.png")
    val dijkstra = Dijkstra(maze)

    val start: Cell = maze.at(0,0).get

    val d = dijkstra.distances(0, 0)
    val maxDist = d.values.max
    val colours = d map {
      case (cell: Cell, distance) => {
        val c = Math.max(0, Math.min(255, 255 - 255 * distance / maxDist))
        val d = 255 - c
        val colour = new Color(c, 120, 120)
        ((cell.row, cell.col) -> colour)
      }
    }

    checkTry(Grid2Png.writeWithColours(maze, outputFile, colours))

  }

}