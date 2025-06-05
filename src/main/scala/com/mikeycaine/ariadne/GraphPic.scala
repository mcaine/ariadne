package com.mikeycaine.ariadne

/**
 * Copyright (c) 2025 Mike Caine
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Any modifications to this file must keep this entire header intact.
 **/

object GraphPic {
  import java.io.File

  // get a random filename
  def tfl: File = {
    var file: File = null
    while {
      def uuid = java.util.UUID.randomUUID.toString.filterNot(_ == '-')
      file = new File(uuid + ".png")
      file.exists
    } do()
    file
  }

  def apply(width: Int = 1000, height:Int = 1000, centre: Point = Point(0.0, 0.0), scale: Int = 50)
  = new GraphPic(width, height, centre, scale)
}

class GraphPic (width: Int, height:Int, centre: Point, scale: Int) {
  import java.awt.image.BufferedImage
  import java.awt.{BasicStroke, Color, Graphics2D}
  import java.io.File
  import javax.imageio.ImageIO

  var points: List[Point] = List[Point]()
  var lines: List[Line] = List[Line]()

  def add(poly: Polygon): Unit = {
    val starts = poly.points
    val ends = poly.points.tail :+ poly.points.head
    val startsAndEnds = starts.zip(ends)
    startsAndEnds.foreach ( se =>
      add(Line.fromPoints(se._1, se._2))
    )
  }

  def add(p: Point): Unit = {
    points = p :: points
  }

  def add(l: Line): Unit = {
    lines = l :: lines
  }

  def scaleToPic(x: Double, y: Double) : (Int, Int) = {
    val imgx = (width / 2) + (x - centre.x) * scale
    val imgy = (height / 2) - (y - centre.y) * scale
    (imgx.round.toInt, imgy.round.toInt)
  }

  def graphicsEndpointsForLine(line: Line): (Int, Int, Int, Int) = {

    if (Math.abs(line.b) > 1e-5 && (Math.abs(line.a / line.b) < 1.0)) {
        val leftX = centre.x - width / (2.0 * scale) // the left hand side of the pic in geom space
        val rightX = centre.x + (width - 1) / (2.0 * scale)
        val leftLine = Line(1, 0, -leftX) // the line up the left hand side in geom space
        val rightLine = Line(1, 0, -rightX) // the line up the left hand side in geom space
        val lp: Point = Line.intersection(line, leftLine)
        val rp: Point = Line.intersection(line, rightLine)
        val (lx, ly) = scaleToPic(lp.x, lp.y)
        val (rx, ry) = scaleToPic(rp.x, rp.y)
        (lx, ly, rx, ry)
    } else {
      val topY = centre.y + height / (2.0 * scale) // the left hand side of the pic in geom space
      val bottomY = centre.y - (height - 1) / (2.0 * scale)
      val topLine = Line(0, 1, -topY) // the line at the top in geom space
      val bottomLine = Line(0, 1, -bottomY) // the line at the bottom in geom space
      val tp: Point = Line.intersection(line, topLine)
      val bp: Point = Line.intersection(line, bottomLine)
      val (tx, ty) = scaleToPic(tp.x, tp.y)
      val (bx, by) = scaleToPic(bp.x, bp.y)
      (tx, ty, bx, by)
    }
  }

  def drawLine(graphics: Graphics2D, line: Line) = {
    val endPoints = graphicsEndpointsForLine(line)
    graphics.drawLine(endPoints._1, endPoints._2 , endPoints._3, endPoints._4)
  }

  def write(file: File = GraphPic.tfl): File = {
    val imageType = BufferedImage.TYPE_INT_RGB
    val img = new BufferedImage(width, height, imageType)
    val graphics: Graphics2D = img.createGraphics()
    //val file = GraphPic.tfl

    val strokeWidth = 1.0f

    graphics.setStroke(new BasicStroke(strokeWidth))
    graphics.setColor(Color.GREEN)
    for (line <- lines) {
      drawLine(graphics, line)
    }

    graphics.setStroke(new BasicStroke(strokeWidth))
    graphics.setColor(Color.RED)
    val pointSize = 8
    for (point <- points) {
      //val imgx = (width / 2) + (point.x - centre.x) * scale
      //val imgy = (height / 2) - (point.y - centre.y) * scale
      val (imgx, imgy) = scaleToPic(point.x, point.y)
      //println (s"imgx = $imgx, imgy = $imgy")
      graphics.fillOval(imgx - (pointSize/2), imgy - (pointSize/2), pointSize, pointSize)
    }

    ImageIO.write(img, "png", file)
    file
  }
}

case class Point(x: Double, y: Double)

// a line of the form ax + by + c = 0
case class Line (a: Double, b: Double, c: Double)
object Line {
  def fromPoints(start: Point, end: Point): Line = {
    //a = (y1 - y2)
    val a = start.y - end.y
    val b = end.x - start.x
    val c = start.x * end.y - end.x * start.y
    this(a,b,c)
  }

  def intersection(first: Line, second: Line): Point = {
    val p = first.a * second.b - first.b * second.a
    //println(s"p is $p")

    val x = (second.c * first.b - first.c * second.b) / (first.a * second.b - first.b * second.a)
    //println(s"x is $x")

    if (Math.abs(first.b) > 1e-5) {
      val y = - ( first.c + first.a * x) / first.b
      Point(x,y)
    } else {
      val y = - ( second.c + second.a * x) / second.b
      Point(x,y)
    }
  }
}

case class Polygon(points: List[Point])

