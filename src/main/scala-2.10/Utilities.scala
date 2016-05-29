import breeze.linalg.DenseMatrix
import breeze.numerics._

/**
  * Created by Yifta on 16-May-16.
  */
object Utilities {
  val DefinitiveStayDistance = 8

  val MaxStayDistance = 30

  val MaxStaySpeed = 0.4


  val r_squared = 40589641000000D
//  def A(a:Int)(b:Int)
  def SquaredHaversineApproximation(longtitude1: DenseMatrix[Double],
                                    latitude1: DenseMatrix[Double],
                                    longtitude2: DenseMatrix[Double],
                                    latitude2: DenseMatrix[Double]): DenseMatrix[Double] = {

    val (lon1Rad, lon2Rad, lat1Rad, lat2Rad) = (toRadians(longtitude1).t,
      toRadians(longtitude2).t,
      toRadians(latitude1),
      toRadians(latitude2))

    val deltaLongtitude:DenseMatrix[Double] = (lon2Rad - lon1Rad) * cos((lat2Rad + lat1Rad) / 2d)
    val deltaLatitude:DenseMatrix[Double] = lat2Rad - lat1Rad

    pow(deltaLongtitude, 2) + pow(deltaLatitude(0,0), 2) * r_squared

  }

  def SquaredHaversineApproximation(longtitude1: Double,
                                    latitude1: Double,
                                    longtitude2: Double,
                                    latitude2: Double): Double = {

    val (lon1Rad, lon2Rad, lat1Rad, lat2Rad) = (toRadians(longtitude1),
      toRadians(longtitude2),
      toRadians(latitude1),
      toRadians(latitude2))
    val deltaLongtitude = (lon2Rad - lon1Rad) * cos((lat2Rad + lat1Rad) / 2)
    val deltaLatitude = lat2Rad - lat1Rad

    pow(deltaLongtitude, 2) + pow(deltaLatitude, 2) * r_squared

  }
}
