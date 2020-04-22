import java.net.InetAddress

import scala.sys.process._
import scala.language.postfixOps
import java.io.File

import scala.io.{BufferedSource, Source}

object Sense extends App {
  case class Command(stringRep: String) {
    val os: String = sys.props("os.name").toLowerCase
    val commandStruct: String = os match {
      case n if n.contains("windows") =>
        (Seq("cmd", "/C") ++ Seq(stringRep)).foldLeft("") { (acc, c) =>
          acc + c + " "
        }
      case _ => stringRep
    }

    def run(): Int = {
      commandStruct !
    }

    def result(): String = {
      commandStruct !!
    }
  }
  case class Camera(host: String, port: String, distFromPoint: Double) {
    def address(): String = "http://" + host + ":" + port + "/"
  }

  case class Ics(host: String, port: String, coordinates: (Double, Double)) {
    def address(): String = "http://" + host + ":" + port + "/"
  }

  def distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val distLon = Math.toRadians(lon2) - Math.toRadians(lon1)
    val distLat = Math.toRadians(lat2) - Math.toRadians(lat1)

    val a: Double = {
      Math.pow(Math.sin(distLat / 2), 2) +
      Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) *
      Math.pow(Math.sin(distLon / 2), 2)
    }

    val c = 2 * Math.asin(Math.sqrt(a))

    val radius = 3956

    c * radius
  }


  val src = Source.fromFile("args.txt")
  val lines = src.getLines().toVector

  val lat = lines(0)
  val lon = lines(1)
  val rad = lines(2)

  val camResult = {
    Command("python kamerka.py --lat " + lat + " --lon " + lon + " --radius " + rad + " --open --camera").result()
  }

  val camAddress = {
    camResult.split("\n")
      .filter(line => line.startsWith("IP: "))
      .map { line =>
        val component = line.split(" - ")
        (component.head.drop(4), component.last)
      }
      .toVector
  }

  val camLocation = {
    camResult.split("\n")
      .filter(line => line.startsWith("Coordinates: "))
      .map(coordinate => coordinate.drop(13))
      .map { point =>
        val latlon = point.split(",")
        (latlon.head, latlon.last)
      }
      .toVector
  }

  val cameras = camAddress.zip(camLocation)
    .map { case ((h, p), (slat, slon)) =>
      Camera(h, p, distance(lat.toDouble, lon.toDouble, slat.toDouble, slon.toDouble))
    }

  val icsResult = {
    Command("python kamerka.py --country US").result()
  }

  val icsAddress = {
    icsResult.split("\n")
      .filter(line => line.startsWith("IP: "))
      .map { line =>
        val component = line.split(" - ")
        (component.head.drop(4), component.last)
      }
      .toVector
  }

  val icsLocation = {
    icsResult.split("\n")
      .filter(line => line.startsWith("Coordinates: "))
      .map(coordinate => coordinate.drop(13))
      .map { point =>
        val latlon = point.split(",")
        (latlon.head.toDouble, latlon.last.toDouble)
      }
      .toVector
  }

  val icss = icsAddress.zip(icsLocation)
    .map { case ((h, p), coordinates) =>
      Ics(h, p, coordinates)
    }

  println(cameras)
  println(icss)
//  val browser = ("firefox http://47.25.244.110:8080/video") !

}
