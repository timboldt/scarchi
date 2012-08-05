package boldt.scarchi.test

import scala.collection.mutable.HashSet

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import boldt.scarchi.Util

class UtilObjectTest extends FunSuite with ShouldMatchers {
  
  test("randomPlusMinusAbsolute 100 +/-10") {
    val list = for (i <- 1 to 1000) yield Util.randomPlusMinusAbsolute(100, 10)
    var set: HashSet[Long] = new HashSet

    for (v <- list) {
      set += v
      v should be >= 90L
      v should be <= 110L
    }
    set should contain (90L)
    set should contain (100L)
    set should contain (110L)
  }
  
  test("randomPlusMinusAbsolute 0 +/-1") {
    val list = for (i <- 1 to 100) yield Util.randomPlusMinusAbsolute(0, 1)
    var set: HashSet[Long] = new HashSet

    for (v <- list) {
      set += v
      v should be >= -1L
      v should be <= 1L
    }
    set should contain (-1L)
    set should contain (0L)
    set should contain (1L)
  }
  
  test("randomPlusMinusPercent 100 +/-10%") {
    val list = for (i <- 1 to 1000) yield Util.randomPlusMinusPercent(100, 10)
    var set: HashSet[Long] = new HashSet

    for (v <- list) {
      set += v
      v should be >= 90L
      v should be <= 110L
    }
    set should contain (90L)
    set should contain (100L)
    set should contain (110L)
  }
  
  test("randomPlusMinusPercent 0 +/-10%") {
    val list = for (i <- 1 to 100) yield Util.randomPlusMinusPercent(0, 10)

    for (v <- list) {
      v should be (0)
    }
  }
  
  test("distanceBetweenPoints") {
    Util.distanceBetweenPoints(0, 0, 3, 4) should be (5)
    Util.distanceBetweenPoints(0, 0, -3, -4) should be (5)
    Util.distanceBetweenPoints(-10, -10, -13, -14) should be (5)
  }
  
  test("diminishedReturn") {
    // Invalid value
    Util.diminishedReturn(0, 100) should be (0L)
    Util.diminishedReturn(-1, 100) should be (0L)
    
    //Invalid scale
    Util.diminishedReturn(100, 0) should be (0L)
    Util.diminishedReturn(100, -1) should be (0L)
    Util.diminishedReturn(-1, -1) should be (0L)
    Util.diminishedReturn(0, 0) should be (0L)
    
    //Varying values against a known scale
    Util.diminishedReturn(1, 12345) should be (2L)
    Util.diminishedReturn(42, 12345) should be (83L)
    Util.diminishedReturn(999, 12345) should be (1750L)
    Util.diminishedReturn(678910, 12345) should be (123444L)

    //With equal values
    Util.diminishedReturn(1, 1) should be (1)
    Util.diminishedReturn(42, 42) should be (42)
    Util.diminishedReturn(32768, 32768) should be (32768)
  }
}
