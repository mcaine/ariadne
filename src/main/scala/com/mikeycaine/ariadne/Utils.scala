package com.mikeycaine.ariadne

import scala.util.Random

object Utils {
  def sample[A](list: List[A]): A = {
    assert(list.nonEmpty)
    val r = new Random()
    val i = r.nextInt(list.size)
    list(i)
  }
}
