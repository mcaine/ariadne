package com.mikeycaine.ariadne

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

  "GraphPic" should "calculate graphics corners for a line" in {
    val gp = GraphPic()

    val (x1, y1, x2, y2) = gp.graphicsEndpointsForLine(line)

    assert(x1 == 0)
    assert(y1 == 750)
    assert(x2 == 1000)
    assert(y2 == 250)
  }

  "GraphPic" should "calculate graphics corners for a vertical line" in {
    val gp = GraphPic()

    val (x1, y1, x2, y2) = gp.graphicsEndpointsForLine(vline)

    assert(x1 == 500)
    assert(y1 == 0)
    assert(x2 == 500)
    assert(y2 == 1000)
  }

  "GraphPic" should "calculate graphics corners for a Steep line" in {
    val gp = GraphPic()

    val (x1, y1, x2, y2) = gp.graphicsEndpointsForLine(steepLine)

    assert(x1 == 450)
    assert(y1 == 0)
    assert(x2 == 550)
    assert(y2 == 1000)
  }

  "GraphPic" should "draw a line correctly" in {
    val gp = GraphPic()
    gp.add(line)
    gp.add(hline)
    gp.add(vline)
    gp.add(steepLine)
    gp.add(start)
    gp.add(end)
    gp.add(Point(-2.0, 0))
    gp.add(Point(2.0, 0))
    val f = gp.write()
    println(s"File is $f")
  }

  "GraphPic" should "draw a line with offset centre" in {
    val gp = GraphPic(centre = Point(0, 5.0))
    gp.add(line)
    gp.add(hline)
    gp.add(vline)
    gp.add(start)
    gp.add(steepLine)
    gp.add(end)
    gp.add(Point(-2.0, 0))
    gp.add(Point(2.0, 0))
    val f = gp.write()
    println(s"File is $f")
  }



}
