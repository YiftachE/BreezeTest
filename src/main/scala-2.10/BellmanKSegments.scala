import breeze.linalg.{DenseVector, DenseMatrix}

/**
  * Created by Yifta on 25-May-16.
  */
object BellmanKSegments {
  def PrepareOneSegments(time: DenseVector[Double], longtitude: DenseVector[Double], latitude: DenseVector[Double], accuracy: Option[DenseVector[Double]] = None): DenseMatrix[Double] = {

    val dataPointsCount = time.length
    val oneSegDistances = DenseMatrix.zeros[Double](dataPointsCount, dataPointsCount)
    for (i <- 0 until dataPointsCount) {
      for (j <- i + 2 until dataPointsCount-1) {
        try {
          val relevantTimes = time(i to j + 1)
          val relevantLon = longtitude(i to j + 1)
          val relevantLat = latitude(i to j + 1)
          val relevantAccuracy = accuracy match {
            case (Some(acc)) => Some(acc(i to j + 1))
            case _ => None
          }
          val (_, _, score, _) = LinearRegressor.FitLinearRegression(relevantTimes.toDenseMatrix, relevantLon.toDenseMatrix, relevantLat.toDenseMatrix, relevantAccuracy)
          oneSegDistances(i, j) = score
        }
        catch {
          case (e: Exception) =>
            println(e)
            println(i)
            println(j)
            null
        }


      }
    }
    oneSegDistances
  }
}
