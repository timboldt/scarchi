package boldt.scarchi

object Util {
  def randomPlusMinusAbsolute(n: Int, delta: Int) =
    (math.random * (2 * delta + 1)).toInt + n - delta

  def randomPlusMinusPercent(n: Int, pct: Int) =
    randomPlusMinusAbsolute(n, (pct.toDouble / 100 * n).toInt)

  def distanceBetweenPoints(x1: Int, y1: Int, x2: Int, y2: Int):Int =
    math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)).toInt
}
