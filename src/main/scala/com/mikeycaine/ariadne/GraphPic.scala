package com.mikeycaine.ariadne

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



  def add(p: Point): Unit = {
    points = p :: points
  }

  def add(l: Line): Unit = {
    lines = l :: lines
  }

  def scaleToPic(x: Double, y: Double) : (Int, Int) = {
    val imgx = (width / 2) + (x - centre.x) * scale
    val imgy = (height / 2) - (y - centre.y) * scale
    (imgx.toInt, imgy.toInt)
  }

  def graphicsEndpointsForLine(line: Line): (Int, Int, Int, Int) = {
    val leftX = centre.x -width / ( 2.0 * scale) // the left hand side of the pic in geom space
    val rightX = centre.x + (width - 1) / (2.0 * scale)
    val leftLine = Line(1, 0, -leftX)  // the line up the left hand side in geom space
    val rightLine = Line(1, 0, -rightX)  // the line up the left hand side in geom space
    val lp: Point = Line.intersection(line, leftLine)
    val rp: Point = Line.intersection(line, rightLine)
    val (lx, ly) = scaleToPic(lp.x, lp.y)
    val (rx, ry) = scaleToPic(rp.x, rp.y)
    (lx, ly, rx, ry)
  }

  def drawLine(graphics: Graphics2D, line: Line) = {
    val endPoints = graphicsEndpointsForLine(line)
    graphics.drawLine(endPoints._1, endPoints._2, endPoints._3, endPoints._4)
  }

  def write(): File = {
    val imageType = BufferedImage.TYPE_INT_RGB
    val img = new BufferedImage(width, height, imageType)
    val graphics: Graphics2D = img.createGraphics()
    val file = GraphPic.tfl

    val strokeWidth = 1.0f

    graphics.setStroke(new BasicStroke(strokeWidth))
    graphics.setColor(Color.GREEN)
    for (line <- lines) {
      drawLine(graphics, line)
    }

    graphics.setStroke(new BasicStroke(strokeWidth))
    graphics.setColor(Color.RED)
    for (point <- points) {
      //val imgx = (width / 2) + (point.x - centre.x) * scale
      //val imgy = (height / 2) - (point.y - centre.y) * scale
      val (imgx, imgy) = scaleToPic(point.x, point.y)
      //println (s"imgx = $imgx, imgy = $imgy")
      graphics.fillOval(imgx.toInt, imgy.toInt, 5, 5)
    }

    ImageIO.write(img, "png", file)
    file
  }
}

case class Point(x: Double, y: Double)

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
    println(s"p is $p")

    val x = (second.c * first.b - first.c * second.b) / (first.a * second.b - first.b * second.a)
    println(s"x is $x")

    if (Math.abs(first.b) > 1e-5) {
      val y = - ( first.c + first.a * x) / first.b
      Point(x,y)
    } else {
      val y = - ( second.c + second.a * x) / second.b
      Point(x,y)
    }
  }
}
