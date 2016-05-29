/**
  * Created by Yifta on 06-May-16.
  */

import breeze.linalg._
import breeze.numerics._

//import com.typesafe.config.{Config, ConfigFactory}

object HelloBreeze {
  def main(args: Array[String]): Unit = {

    val times = (1 to 1000).map(_.toDouble)
    val longtitudes = (1 to 1000).map(e => 30 + e.toDouble / 1000)
    val latitudes = times.zip(longtitudes).map { case (time, longitude) =>
      (longitude * 2 + 6 - time) / 4
    }
    val denseVectorTimes = new DenseVector[Double](times.toArray)
    val denseVectorLongitude = new DenseVector[Double](longtitudes.toArray)
    val denseVectorLatitude = new DenseVector[Double](latitudes.toArray)
    val oneSeg = time {
      BellmanKSegments.PrepareOneSegments(denseVectorTimes, denseVectorLongitude, denseVectorLatitude)
    }

    val all = oneSeg.findAll(_ > 0.0001)
//    println(all)
//    println(all.size)
+
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed Time: " + (t1 - t0) + "ns")
    result
  }
}
