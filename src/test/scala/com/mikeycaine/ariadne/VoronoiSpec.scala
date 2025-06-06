package com.mikeycaine.ariadne

import java.io.File
import java.util.Random

class VoronoiSpec  extends AriadneBaseSpec {
  val h = 10
  val w = 10
  val s = 50 // image scale, no. of pixels equal to 1.0 in geom space

  "something" should "work" in {

    val nPoints = 3
    val rand = new Random()
    val points = for {
      i <- 0 until nPoints
    } yield {
      Point(-w + 2.0 * rand.nextDouble * w, -h + rand.nextDouble * h)
    }

    val gp = GraphPic(width = w * 2 * s, 2 * h * s, scale = s)

    points foreach {
      gp.add(_)
    }

//    val pairs = for {
//      i <- 1 to nPoints
//      j <- 1 to nPoints
//      if (i < j)
//    } yield (i, j)

    val bisectors = for {
      i <- 0 until nPoints
      j <- 0 until nPoints
      if (i < j)
    } yield {
      val a = points(i)
      val b = points(j)
      val lineThrough = Line.fromPoints(a, b)
      //gp.add(lineThrough)

      val ob = Line.orthogonalBisector(a, b)
      gp.add(ob)
      ob
    }

    //assert(bisectors.size == nPoints - 1)

    for {
      i <- 0 until bisectors.size
      j <- 0 until bisectors.size
      if (i < j)
    } {
      val a = bisectors(i)
      val b = bisectors(j)
      val p = Line.intersection(a, b)
      //gp.add(p)
    }

    val f = gp.write(new File("vor.png"))
    println(s"File is $f")






  }

}
