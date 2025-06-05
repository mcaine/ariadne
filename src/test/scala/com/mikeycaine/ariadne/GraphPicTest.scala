package com.mikeycaine.ariadne

import java.io.File

class GraphPicTest extends AriadneBaseSpec {

  // a line of slope 1/2 through the origin
  val start = Point(-2, -1)
  val end = Point(2, 1)
  val line = Line.fromPoints(start, end)

  // a horizontal line
  val hline = Line.fromPoints(Point(-1.0, 0.0), Point(1.0, 0.0))

  // a vertical line through the origin
  val vline = Line.fromPoints(Point(0.0, -1.0), Point(0.0, 1.0))

  // a steep negative slope
  val steepLine = Line.fromPoints(Point(-1, 10.0), Point(1.0, -10.0))

  // a square
  val square = Polygon(List(Point(-1,-1), Point(-1, 1), Point(1,1), Point (1, -1)))

  def nAgonPoints(n: Int, size: Double = 1.0) = (0 until n).map(i =>
      Point(size * Math.cos(i * 2 * Math.PI / n), size * Math.sin(i * 2 * Math.PI / n))
  ).toList
  val pentagon = Polygon(nAgonPoints(5, 3.0))

  "GraphPic" should "calculate graphics corners for a line" in {
    val gp = GraphPic()

    val (x1, y1, x2, y2) = gp.graphicsEndpointsForLine(line)

    assert(x1 == 0)
    assert(y1 == 750)
    assert(x2 == 1000)
    assert(y2 == 250)
  }

  it should "calculate graphics corners for a vertical line" in {
    val gp = GraphPic()

    val (x1, y1, x2, y2) = gp.graphicsEndpointsForLine(vline)

    assert(x1 == 500)
    assert(y1 == 0)
    assert(x2 == 500)
    assert(y2 == 1000)
  }

  it should "calculate graphics corners for a Steep line" in {
    val gp = GraphPic()

    val (x1, y1, x2, y2) = gp.graphicsEndpointsForLine(steepLine)

    assert(x1 == 450)
    assert(y1 == 0)
    assert(x2 == 550)
    assert(y2 == 1000)
  }

  it should "calculate line for a backwards horizontal line"  in {
    val l = Line.fromPoints(Point(1.0,-1.0), Point(-1.0,-1.0))
    println(l)
  }

  it should "draw a line correctly" in {
    val gp = GraphPic()
    gp.add(line)
    gp.add(hline)
    gp.add(vline)
    gp.add(steepLine)
    gp.add(start)
    gp.add(end)
    gp.add(Point(-2.0, 0))
    gp.add(Point(2.0, 0))
    val f = gp.write(new File("correctLine.png"))
    println(s"File is $f")
  }

  it should "draw a line with offset centre" in {
    val gp = GraphPic(centre = Point(0, 5.0))
    gp.add(line)
    gp.add(hline)
    gp.add(vline)
    gp.add(start)
    gp.add(steepLine)
    gp.add(end)
    gp.add(Point(-2.0, 0))
    gp.add(Point(2.0, 0))
    val f = gp.write(new File("offset.png"))
    println(s"File is $f")
  }

  it should "draw a square" in {
    val gp = GraphPic()
    gp.add(square)
    for (p <- square.points) {
      gp.add(p)
    }

    val f = gp.write(new File("square.png"))
    println(s"File is $f")
  }

  it should "draw a pentagon" in {
    val gp = GraphPic()
    gp.add(pentagon)
    for (p <- pentagon.points) {
      gp.add(p)
    }

    val f = gp.write(new File("pentagon.png"))
    println(s"File is $f")
  }



}
