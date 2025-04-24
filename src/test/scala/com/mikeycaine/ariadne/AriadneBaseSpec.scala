package com.mikeycaine.ariadne

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

class AriadneBaseSpec extends AnyFlatSpec with Matchers {

  def checkTry[F](t: Try[F]): Unit = t match {
    case Success(_) =>
    case Failure(exception) => fail(exception.getMessage)
  }
  
}
