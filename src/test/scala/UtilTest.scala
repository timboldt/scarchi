package boldt.scarchi.test

import scala.collection.mutable.HashSet
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import boldt.scarchi.Util

class UtilObjectTest extends FunSuite with ShouldMatchers {
  
  test("randomPlusMinusAbsolute 100 +/-10") {
    val list = for (i <- 1 to 1000) yield Util.randomPlusMinusAbsolute(100, 10)
    var set: HashSet[Int] = new HashSet

    for (v <- list) {
      set += v
      v should be >= 90
      v should be <= 110
    }
    set should contain (90)
    set should contain (100)
    set should contain (110)
  }
  
  test("randomPlusMinusAbsolute 0 +/-1") {
    val list = for (i <- 1 to 100) yield Util.randomPlusMinusAbsolute(0, 1)
    var set: HashSet[Int] = new HashSet

    for (v <- list) {
      set += v
      v should be >= -1
      v should be <= 1
    }
    set should contain (-1)
    set should contain (0)
    set should contain (1)
  }
  
  test("randomPlusMinusPercent 100 +/-10%") {
    val list = for (i <- 1 to 1000) yield Util.randomPlusMinusPercent(100, 10)
    var set: HashSet[Int] = new HashSet

    for (v <- list) {
      set += v
      v should be >= 90
      v should be <= 110
    }
    set should contain (90)
    set should contain (100)
    set should contain (110)
  }
  
  test("randomPlusMinusPercent 0 +/-10%") {
    val list = for (i <- 1 to 100) yield Util.randomPlusMinusPercent(0, 10)

    for (v <- list) {
      v should be (0)
    }
  }
  
  test("distanceBetweenPoints") {
    Util.distanceBetweenPoints(0, 0, 3, 4) should be (5)
  }
}
