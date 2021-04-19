/*
 *  Translate.java
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

import java.awt.datatransfer.{Clipboard, ClipboardOwner, DataFlavor, StringSelection, Transferable}
import java.awt.event.InputEvent.BUTTON1_DOWN_MASK
import java.awt.event.KeyEvent.{VK_A, VK_BACK_SPACE, VK_CONTROL, VK_V}
import java.awt.{EventQueue, GraphicsEnvironment, Robot, Toolkit}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.{Timer, TimerTask}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Future, Promise}
import scala.io.Source
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object Translate {
  case class Point(x: Int, y: Int)

  case class Config(
                     input: File, output: Option[File], srt: Boolean, charset: String,
                     langInPt: Option[Point],
                     langOutPt: Option[Point],
                     delay: Int,
                   )

  case class Group(header: Seq[String], body: Seq[String])

  def main(args: Array[String]): Unit = {
    object parse extends ScallopConf(args.toSeq) {
      printedName = "Translate"
      // cf. https://en.wikipedia.org/wiki/SubRip
      val srt: Opt[Boolean] = toggle()
      val output: Opt[File] = opt()
      val charset: Opt[String] = opt(default = Some(UTF_8.name()))
      val langInPt: Opt[List[Int]] = opt(validate = _.size == 2)
      val langOutPt: Opt[List[Int]] = opt(validate = _.size == 2)
      val delay: Opt[Int] = opt(default = Some(4))
      val input: Opt[File] = trailArg()
      verify()

      private def mkPt(in: Opt[List[Int]]): Option[Point] =
        in.toOption.flatMap {
          case x :: y :: Nil => Some(Point(x, y))
          case _ => None
        }

      val conf: Config = Config(input = input(), output = output.toOption, srt = srt.getOrElse(input().extL == "srt"),
        charset = charset(), langInPt = mkPt(langInPt), langOutPt = mkPt(langOutPt),
        delay = delay(),
      )
    }
    run(parse.conf)
  }

  def run(conf: Config): Unit = {
    val src = Source.fromFile(conf.input, conf.charset)
    val linesIn0 = try {
      src.getLines().toIndexedSeq
    } finally {
      src.close()
    }
    val linesIn = linesIn0 match {
      case head +: tail if head.startsWith("\uFEFF") =>
        require(conf.charset.toUpperCase == UTF_8.name(), "File contains Byte Order Mark but charset is not UTF-8")
        head.tail +: tail

      case other => other
    }

    val groups: Seq[Group] = if (!conf.srt) Group(Nil, linesIn) :: Nil else {
      val b = Seq.newBuilder[Group]
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
        b += Group(header, body.result())
        gCnt += 1
      }
      b.result()
    }

    println(s"num lines = ${linesIn.size}; num groups = ${groups.size}")

    implicit val timer: Timer = new Timer("robot", false)

    println("Bring the browser with page 'https://translate.google.com/' to front.")
    println(s"Stop touching the mouse and keyboard after ${conf.delay} seconds!")
    after(conf.delay)(perform(groups, conf))
  }

  def after[A](sec: Double = 1.0)(fun: => A)(implicit timer: Timer): Future[A] = {
    val pr = Promise[A]()
    val tt = new TimerTask {
      override def cancel(): Boolean = {
        pr.tryFailure(new Exception("Cancelled"))
        super.cancel()
      }

      override def run(): Unit =
        EventQueue.invokeLater(() => {
          try {
            val res = fun
            pr.trySuccess(res)
          } catch {
            case NonFatal(ex) => pr.tryFailure(ex)
          }
        })
    }
    timer.schedule(tt, (sec * 1000).toLong)
    pr.future
  }

  def perform(groups: Seq[Group], conf: Config)(implicit timer: Timer): Unit = {
    println("Beginning translation...")
    val sd = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
    val gc = sd.getDefaultConfiguration
    val gr = gc.getBounds
    val r = new Robot(sd)
    val langInPt = conf.langInPt.getOrElse(Point(gr.getCenterX.toInt - 100, gr.y + 360))
    val langOutPt = conf.langOutPt.getOrElse(Point(gr.x + gr.width - 450, gr.y + 420))
    val clip = Toolkit.getDefaultToolkit.getSystemClipboard
    var clipMine = false
    object owner extends ClipboardOwner {
      override def lostOwnership(clipboard: Clipboard, contents: Transferable): Unit = {
        clipMine = false
      }
    }

    def setClip(txt: String): Unit = {
      val trans = new StringSelection(txt)
      clip.setContents(trans, owner)
      clipMine = true
    }

    setClip(
      """"El tiempo se nos echa
        |encima y solo tienes unas horas.
        |""".stripMargin)

    import r._
    setAutoDelay(4)

    def keyType(codes: Int*): Unit = {
      codes.foreach(keyPress)
      codes.reverseIterator.foreach(keyRelease)
    }

    def mouseClick(code: Int = BUTTON1_DOWN_MASK): Unit = {
      mousePress(code)
      mouseRelease(code)
    }

    mouseMove(langInPt.x, langInPt.y)
    mouseClick()
    keyType(VK_CONTROL, VK_A)
    keyType(VK_BACK_SPACE)
    keyType(VK_CONTROL, VK_V)

    val fut = for {
      _ <- after() {
        mouseMove(langOutPt.x, langOutPt.y)
        mouseClick()
      }
      res <- after() {
        val d = clip.getData(DataFlavor.stringFlavor)
        require(!clipMine, "Clipboard was not replaced")
        d.toString
      }
    } yield res

    fut.onComplete {
      case Success(res) =>
        println(s"Translation: $res")
        sys.exit()

      case Failure(ex) =>
        ex.printStackTrace()
        sys.exit(1)
    }
  }
}
