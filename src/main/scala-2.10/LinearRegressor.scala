import breeze.linalg._
import breeze.numerics._

object LinearRegressor {

  def FitLinearRegression(time: DenseMatrix[Double], longtitude: DenseMatrix[Double], latitude: DenseMatrix[Double], accuracy: Option[DenseVector[Double]] = None):
  (DenseMatrix[Double], DenseMatrix[Double], Double, Boolean) = {
    val startTime = time(0, 0)
    time -= startTime
    val biasTime = DenseMatrix.vertcat(DenseMatrix.ones[Double](time.rows, time.cols), time)
    val (weights, weightsMat) = accuracy match {
      case Some(acc) =>
        val tempWeight = pow(acc, -2)
        (tempWeight, diag(tempWeight))
      case _ =>
        (DenseVector.ones[Double](time.cols), DenseMatrix.eye[Double](time.cols))
    }

    val timePseudoInverted: DenseMatrix[Double] = inv((biasTime * weightsMat) * biasTime.t) * (biasTime * weightsMat)
    val thetaLongitude: DenseMatrix[Double] = timePseudoInverted * longtitude.t
    val thetaLatitude: DenseMatrix[Double] = timePseudoInverted * latitude.t

    val (score, isStay) = CalculateSegmentScore(time, longtitude, latitude, thetaLongitude, thetaLatitude, weights)
    time :+= startTime

    thetaLongitude(0,::) -= thetaLongitude(1,::)*startTime
    thetaLatitude(0,::) -= thetaLongitude(1,::)*startTime

    (thetaLongitude, thetaLatitude, score, isStay)
  }

  def PredictLinearRegression(time: DenseMatrix[Double], theta: DenseMatrix[Double]): DenseMatrix[Double] = {
    val transposedTheta = theta.t
    val timeMat = time match {
      case t if t.rows == 1 =>
        DenseMatrix.vertcat(DenseMatrix.ones[Double](1, t.cols), t)
      case t if t.rows != 2 =>
        val flatTime = t.flatten()
        DenseMatrix.vertcat(DenseMatrix.ones[Double](1, flatTime.length), flatTime.toDenseMatrix)
      case _ => time
    }
    transposedTheta * timeMat
  }

  def IsSegmentStay(longtitudePredictions: DenseVector[Double],
                    latitudePredictions: DenseVector[Double],
                    time: DenseVector[Double]): Boolean = {

    val totalDistance: Double = Utilities.SquaredHaversineApproximation(
      longtitudePredictions(longtitudePredictions.length - 1),
      latitudePredictions(latitudePredictions.length - 1),
      longtitudePredictions(0),
      latitudePredictions(0))

    val v = totalDistance / (time(time.length - 1) - time(0))
    if ((v < Utilities.MaxStaySpeed && totalDistance < Utilities.MaxStayDistance)
      || (totalDistance < Utilities.DefinitiveStayDistance))
      true
    else
      false
  }


  def CalculateSegmentScore(time: DenseMatrix[Double],
                            longtitude: DenseMatrix[Double],
                            latitude: DenseMatrix[Double],
                            thetalongtitude: DenseMatrix[Double],
                            thetaLatitude: DenseMatrix[Double],
                            weights: DenseVector[Double]): (Double, Boolean) = {

    val longtitudePredictions = PredictLinearRegression(time, thetalongtitude)
    val latitudePredictions = PredictLinearRegression(time, thetaLatitude)

    val distanceSquared = Utilities.SquaredHaversineApproximation(longtitude, latitude, longtitudePredictions, latitudePredictions).t

    val calculatedVal:DenseMatrix[Double]= weights.toDenseMatrix * distanceSquared

    val score: Double= 0.5 * (calculatedVal(0,0) + time.cols * log(2) - sum(log(weights)))

    (score, IsSegmentStay(longtitudePredictions.toDenseVector, latitudePredictions.toDenseVector, time.toDenseVector))

  }

}
