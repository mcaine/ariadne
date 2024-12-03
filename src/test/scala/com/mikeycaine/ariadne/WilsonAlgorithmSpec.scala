package com.mikeycaine.ariadne

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WilsonAlgorithmSpec extends AnyFlatSpec with Matchers {

  "WilsonAlgorithm" should "return a maze" in {
    val gridMaze: GridMaze = WilsonAlgorithm(10, 10)
    
  }

}