package com.mikeycaine.ariadne

import com.github.dragon66.AnimatedGIFWriter

import java.awt.image.BufferedImage
import java.io.FileOutputStream
import java.nio.file.{Path, Paths}
import scala.util.Try

object GIFWriter {

  def writeGIF(filename: String, frame: Int => BufferedImage, nFrames: Int = 1): Try[Path] = Try {
    val path: Path = Paths.get(filename)
    val gifOS = new FileOutputStream(path.toFile)
    val gifWriter = new AnimatedGIFWriter(true)

    gifWriter.prepareForWrite(gifOS, -1, -1);

    for (i <- 1 to nFrames) {
      val img: BufferedImage = frame(i)
      gifWriter.writeFrame(gifOS, img)
    }

    gifWriter.finishWrite(gifOS)

    path
  }
}
