/*
 *  Join.java
 *  (SubtitleTools)
 *
 *  Copyright (c) 2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.subtitles

import de.sciss.file._
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import java.io.FileOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import scala.io.Source

object Join {
  case class Config(
                     inputs: Seq[File], output: File, charset: String, forceOverwrite: Boolean, verbose: Boolean
                   )

  case class Group(header: Seq[String], body: Seq[String]) {
    def toTitle: Title = {
      val Seq(idS, timeSpanS) = header
      val id        = idS.toInt
      val timeSpan  = TimeSpan.parse(timeSpanS)
      Title(id = id, time = timeSpan, body = body)
    }
  }

  case class Title(id: Int, time: TimeSpan, body: Seq[String]) {
    def mkString: String = {
      val bodyS = body.mkString("\n")
      s"""$id
         |$time
         |$bodyS
         |""".stripMargin
    }
  }

  object TimeCode {
    def parse(s: String): TimeCode = {
      // 00:00:38,306
      val Array(hS, mS, smS) = s.split(":")
      val hoursV    = hS.toInt
      val minutesV  = mS.toInt
      val Array(sS, mlS) = smS.split(",")
      val secondsV  = sS.toInt
      val millisV   = mlS.toInt
      val millis    = ((hoursV * 60 + minutesV) * 60 + secondsV) * 1000 + millisV
      TimeCode(millis)
    }
  }
  case class TimeCode(millis: Int) {
    override def toString: String = {
      val millisV   = millis % 1000
      val seconds   = millis / 1000
      val secondsV  = seconds % 60
      val minutes   = seconds / 60
      val minutesV  = minutes % 60
      val hoursV    = minutes / 60
      require (hoursV < 100)
      f"$hoursV%02d:$minutesV%02d:$secondsV%02d,$millisV%03d"
    }
  }

  object TimeSpan {
    def parse(s: String): TimeSpan = {
      val Array(startS, stopS) = s.split(" --> ")
      val start = TimeCode.parse(startS.trim)
      val stop  = TimeCode.parse(stopS .trim)
      TimeSpan(start = start, stop = stop)
    }
  }
  case class TimeSpan(start: TimeCode, stop: TimeCode) {
    override def toString: String = s"$start --> $stop"
  }

  def main(args: Array[String]): Unit = {
    object parse extends ScallopConf(args.toSeq) {
      printedName = "Join"
      // cf. https://en.wikipedia.org/wiki/SubRip
      val output  : Opt[File]       = opt(required = true)
      val charset : Opt[String]     = opt(default = Some(UTF_8.name()))
      val force   : Opt[Boolean]    = toggle(default = Some(false))
      val verbose : Opt[Boolean]    = toggle(default = Some(false))
      val inputs  : Opt[List[File]] = trailArg(required = true)
      verify()

      val conf: Config = Config(inputs = inputs(), output = output(),
        charset = charset(), forceOverwrite = force(), verbose = verbose())
    }
    run(parse.conf)
  }

  object Titles {
    def read(f: File, charset: String): Titles = {
      val src = Source.fromFile(f, charset)
      val linesIn0 = try {
        src.getLines().toIndexedSeq
      } finally {
        src.close()
      }
      val (bom, linesIn) = linesIn0 match {
        case head +: tail if head.startsWith("\uFEFF") =>
          require(charset.toUpperCase == UTF_8.name(), "File contains Byte Order Mark but charset is not UTF-8")
          (true, head.tail +: tail)

        case other => (false, other)
      }

      val titles: Seq[Title] = {
        val b = Seq.newBuilder[Title]
        val it = linesIn.iterator
        var gCnt = 1
        while (it.hasNext) {
          val index = it.next().trim
          require(index.toIntOption.isDefined, s"'$index' in group $gCnt")
          require(it.hasNext)
          val times = it.next()
          require(times.contains("-->"))
          require(it.hasNext)
          val body = Seq.newBuilder[String]
          while ( {
            val ln = it.next()
            ln.trim.nonEmpty && {
              body += ln
              it.hasNext
            }
          }) ()
          val header = index :: times :: Nil
          val g = Group(header, body.result())
          b += g.toTitle
          gCnt += 1
        }
        b.result()
      }

      Titles(byteOrderMark = bom, charset = charset, contents = titles)
    }
  }
  case class Titles(byteOrderMark: Boolean, charset: String, contents: Seq[Title]) {
    def write(f: File): Unit = {
      val fOut = new FileOutputStream(f)
      try {
        val head      = if (byteOrderMark) "\uFEFF" else ""
        val contentsS = contents.map(_.mkString).mkString(head, "\n", "")
        fOut.write(contentsS.getBytes(charset))
      } finally {
        fOut.close()
      }
    }
  }

  def run(conf: Config): Unit = {
    if (conf.output.exists() && !conf.forceOverwrite) {
      Console.err.println(s"Output file ${conf.output} already exists. Not overwriting!")
      sys.exit(1)
    }

    val sources: Seq[Titles] = conf.inputs.map { input =>
      val tt = Titles.read(input, conf.charset)
      if (conf.verbose) {
        println(s"for ${input.base}: num titles = ${tt.contents.size}")
      }
      tt
    }

    val renumbered = sources.iterator.flatMap(_.contents).zipWithIndex.map { case (title, id0) =>
      title.copy(id = id0 + 1)
    } .toSeq

    val titlesOut = sources.head.copy(contents = renumbered)
    titlesOut.write(conf.output)
  }
}
