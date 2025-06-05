package com.mikeycaine.ariadne

class GraphPicTest extends AriadneBaseSpec {

  // a line of slope 1/2 through the origin
  val start = Point(-2, -1)
  val end = Point(2, 1)
  val line = Line.fromPoints(start, end)

  // a horizontal line
  val hline = Line.fromPoints(Point(-1.0, 0.0), Point(1.0, 0.0))

  "GraphPic" should "calculate graphics corners for a line" in {
    val gp = GraphPic()

    val (x1, y1, x2, y2) = gp.graphicsEndpointsForLine(line)

    assert(x1 == 0)
    assert(y1 == 750)
    assert(x2 == 999)
    assert(y2 == 250)
  }

  "GraphPic" should "draw a line correctly" in {
    val gp = GraphPic()
    gp.add(line)
    gp.add(hline)
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
    gp.add(start)
    gp.add(end)
    gp.add(Point(-2.0, 0))
    gp.add(Point(2.0, 0))
    val f = gp.write()
    println(s"File is $f")
  }



}
