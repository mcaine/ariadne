package com.mikeycaine.ariadne

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D}
import java.io.File
import javax.imageio.ImageIO
import scala.util.Try

object Grid2Png {

  val gridScaleFactor: Int = 20
  val border: Int = gridScaleFactor / 2
  val strokeWidth: Int = 4
  val doubleBorder: Int = 2 * border

  def bufferedImageForGrid(grid: GridMaze, colours: Map[(Int, Int), Color] = Map()): BufferedImage = {
    val imageWidth = grid.cols * gridScaleFactor + doubleBorder + 1
    val imageHeight = grid.rows * gridScaleFactor + doubleBorder + 1

    val imageType = BufferedImage.TYPE_INT_RGB
    val img = new BufferedImage(imageWidth, imageHeight, imageType)
    val graphics: Graphics2D = img.createGraphics()

    graphics.setColor(Color.WHITE)
    graphics.fillRect(0, 0, imageWidth, imageHeight)

    for {
      ((row, column), colour) <- colours
    } {
      val (x1, y1, x2, y2) = corners(row, column)
      graphics.setColor(colour)
      graphics.fillRect(x1, y1 , gridScaleFactor, gridScaleFactor)
    }

    graphics.setStroke(new BasicStroke(strokeWidth))

    for {
      row <- 0 until grid.rows
      column <- 0 until grid.cols
    } {
      val (x1, y1, x2, y2) = corners(row, column)
      grid.at(row, column).foreach { cell =>
        graphics.setColor(Color.BLACK)
        if (!grid.canGoNorthFrom(cell)) graphics.drawLine(x1, y1, x2, y1)
        if (!grid.canGoSouthFrom(cell)) graphics.drawLine(x1, y2, x2, y2)
        if (!grid.canGoWestFrom(cell)) graphics.drawLine(x1, y1, x1, y2)
        if (!grid.canGoEastFrom(cell)) graphics.drawLine(x2, y1, x2, y2)
      }
    }

    graphics.dispose()
    img
  }

  def write (grid: GridMaze, outputFile: File): Try[File] = Try {
    val img = bufferedImageForGrid(grid)

    ImageIO.write(img, "png", outputFile)
    outputFile
  }

  def writeWithColours (grid: GridMaze, outputFile: File, colours: Map[(Int, Int), Color]): Try[File] = Try {
    val img = bufferedImageForGrid(grid, colours)

    ImageIO.write(img, "png", outputFile)
    outputFile
  }

  def corners(row: Int, col: Int): (Int, Int, Int, Int) = {
    val x1 = border + col * gridScaleFactor
    val y1 = border + row * gridScaleFactor

    val x2 = x1 + gridScaleFactor
    val y2 = y1 + gridScaleFactor

    (x1, y1, x2, y2)
  }

}
