package com.mikeycaine.ariadne

import com.github.dragon66.AnimatedGIFWriter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.Files
import java.util.Random
import scala.util.{Failure, Success, Try}


class GifWriterSpec extends AriadneBaseSpec {

  "something" should "do something" in {

    val size = 50
    val maze: GridMaze = HunterKillerAlgorithm(size, size)
    val dijkstra = Dijkstra(maze)

    val d: Map[Cell, Int] = dijkstra.distances(0, 0)

    val start = maze.at(0,0).get
    val end = maze.at(size - 1, size - 1).get

    val path: Seq[Cell] = RouteSearch(maze).startAt(start, end)
    val pathLength = path.length

    val maxDist = d.values.max
    println(s"Max distance is $maxDist")
    println(s"Path length is $pathLength")

    val gifWriter = new AnimatedGIFWriter(true)
    val gifOutStream = new FileOutputStream("gggifwriterspec.gif")
    gifWriter.prepareForWrite(gifOutStream, -1, -1);

    for (
      i <- 1 to pathLength
    ) {
      val dijkstraColours = GridMaze.distanceColours(d.filter((k,v) => v < i))

      val routeColours = path.take(i).map(cell => ((cell.row, cell.col) -> Color.RED)).toMap
      val allColours = dijkstraColours ++ routeColours

      Grid2Png.writeWithColours(maze, new File(s"gifwriter${i}.png"), allColours).map( file =>
        val fin = new FileInputStream(file);
        val image = javax.imageio.ImageIO.read(fin);
        fin.close();
        Files.delete(file.toPath)
        gifWriter.writeFrame(gifOutStream, image);
      )

    }

  }
}