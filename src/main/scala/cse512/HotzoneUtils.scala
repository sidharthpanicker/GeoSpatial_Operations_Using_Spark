package cse512

object HotzoneUtils {

  def  ST_Contains(queryRectangle:String, pointString:String):Boolean = {
    try {
      var rectangePoints: Array[String] = queryRectangle.split(",")
      var pointToBeSearched: Array[String] = pointString.split(",")
      var rectangePointsDouble: Array[Double] = new Array[Double](4)
      var pointToBeSearchedDouble: Array[Double] = new Array[Double](2)


      for (a <- 0 to 3) {
        rectangePointsDouble(a) = rectangePoints(a).trim().toDouble
      }
      for (a <- 0 to 1) {
        pointToBeSearchedDouble(a) = pointToBeSearched(a).trim().toDouble
      }

      var minX = math.min(rectangePointsDouble(0), rectangePointsDouble(2))
      var maxX = math.max(rectangePointsDouble(0), rectangePointsDouble(2))
      var minY = math.min(rectangePointsDouble(1), rectangePointsDouble(3))
      var maxY = math.max(rectangePointsDouble(1), rectangePointsDouble(3))

      var px = pointToBeSearchedDouble(0)
      var py = pointToBeSearchedDouble(1)

      if (px < minX || px > maxX || py < minY || py > maxY)
        return false
      else
        return true
    }
    catch{
      case _: Throwable =>return false
    }
    return false
  }
  // YOU NEED TO CHANGE THIS PART
}