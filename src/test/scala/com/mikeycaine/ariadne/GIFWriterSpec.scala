package com.mikeycaine.ariadne

import com.mikeycaine.ariadne.Grid2Png.bufferedImageForGrid

import java.awt.Color
import java.awt.image.BufferedImage

class GIFWriterSpec extends AriadneBaseSpec {
  "GIFWriter" should "write a GIF" in {
    val size = 50
    val maze: GridMaze = HunterKillerAlgorithm(size, size)

    val distances: Map[Cell, Int] = Dijkstra(maze).distances(0, 0)
    val start = maze.at(0, 0).get
    val end = maze.at(size - 1, size - 1).get
    val path: Seq[Cell] = RouteSearch(maze).startAt(start, end)
    val pathLength = path.length
    //val maxDist = distances.values.max

    val nFrames = pathLength

    val dijkstraColours = GridMaze.distanceColours(distances)

    val frameGenerator: Int => BufferedImage = (i: Int) => {
      val routeColours = path.take(i).map(cell => ((cell.row, cell.col) -> Color.RED)).toMap
      bufferedImageForGrid(maze, dijkstraColours ++ routeColours)
    }

    val result = GIFWriter.writeGIF("xxx.gif", frameGenerator, nFrames)
    assert(result.isSuccess)
    //result.map(p => println(s"Created $p"))
  }
}