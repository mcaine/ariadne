package com.mikeycaine.ariadne

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GridMazeSpec extends AnyFlatSpec with Matchers {

  "GridMaze" should "at least construct" in {
    val gridMaze = GridMaze(10, 10)
  }

  it should "find a cell that exists" in {
    val gridMaze = GridMaze(10, 10)

    assert(gridMaze.at((0,0)).isDefined)
  }

  it should "not find a cell that doesn't exist" in {
    val gridMaze = GridMaze(10, 10)

    assert(gridMaze.at((-2, -2)).isEmpty)
  }

  it should "respect links" in {
    val gridMaze = GridMaze(10, 10)

    val first = gridMaze.at((0,0)).get
    val second = gridMaze.at((0,1)).get
    val third = gridMaze.at((0,2)).get

    assert(!first.isLinkedTo(second))
    assert(!first.isLinkedTo(Some(second)))
    assert(!first.isLinkedTo(third))
    assert(!first.isLinkedTo(Some(third)))
    assert(!second.isLinkedTo(third))
    assert(!second.isLinkedTo(Some(third)))

    first.link(second)

    assert(first.isLinkedTo(second))
    assert(first.isLinkedTo(Some(second)))
    assert(!first.isLinkedTo(Some(third)))
    assert(!first.isLinkedTo(third))
    assert(!second.isLinkedTo(third))
    assert(!second.isLinkedTo(Some(third)))
  }
}