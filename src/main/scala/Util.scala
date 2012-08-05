package boldt.scarchi

object Util {
  def randomPlusMinusAbsolute(n: Long, delta: Long) =
    (math.random * (2 * delta + 1)).toLong + n - delta

  def randomPlusMinusPercent(n: Long, pct: Long) =
    randomPlusMinusAbsolute(n, (pct.toDouble / 100 * n).toLong)

  def distanceBetweenPoints(x1: Long, y1: Long, x2: Long, y2: Long):Long =
    math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)).toLong
    
  def diminishedReturn(value:Long, scale:Long) =
    if (value <= 0 || scale <=0) {
      0
    }
    else {
      val factor = value.toDouble / scale.toDouble
      val position = (math.sqrt(8.0 * factor + 1) - 1) / 2
      math.round(scale.toDouble * position)
    }
}
