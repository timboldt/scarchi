package boldt.scarchi.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import boldt.scarchi.Island

class IslandTest extends FunSuite with ShouldMatchers {
  
  def testIsland(
    population:Long=100L, happiness:Long=100L,
    foodStored:Long=100L, rawMatStored:Long=100L, goodsStored:Long=100L,
    foodProductionFactor:Long=100L, rawMatProductionFactor:Long=100L, goodsProductionFactor:Long=100L
  ) =
    new Island(
      population = population, 
      happiness = happiness,
      foodStored = foodStored,
      rawMatStored = rawMatStored,
      goodsStored = goodsStored,
      foodProductionFactor = foodProductionFactor,
      rawMatProductionFactor = rawMatProductionFactor,
      goodsProductionFactor = goodsProductionFactor,
      genome = "ABCD-ABCD-ABCD",
      x = 1,
      y = 1)
      
  test("eoyPopulation") {
    (testIsland(foodStored=100).eoyPopulation
    ) should (be >= (100L) and be <= (105L))

    (testIsland(foodStored=0).eoyPopulation
    ) should (be >= (24L) and be < (50L))

    (testIsland(population=1,foodStored=0).eoyPopulation
    ) should (be >= (0L) and be <= (1L))

    (testIsland(population=1,foodStored=1).eoyPopulation
    ) should (be >= (1L) and be <= (2L))
  }
  
  test("eoyHappiness") {
    (testIsland().eoyHappiness
    ) should be (112L)

    (testIsland(goodsStored=99, foodStored=99).eoyHappiness
    ) should be (100L)

    (testIsland(happiness=0).eoyHappiness
    ) should be (12L)

    (testIsland(happiness=0, goodsStored=50, foodStored=50).eoyHappiness
    ) should be (0L)

    (testIsland(happiness=200, goodsStored=200, foodStored=200).eoyHappiness
    ) should be (200L)
  }
  
  test("eoyFoodProduced") {

    (testIsland().eoyFoodProduced(100)
    ) should be (200L)

    (testIsland(population=50,foodProductionFactor=50).eoyFoodProduced(100)
    ) should be (100L)

    (testIsland().eoyFoodProduced(0)
    ) should be (0L)

    (testIsland(population=0).eoyFoodProduced(100)
    ) should be (0L)

    (testIsland(population=100,foodProductionFactor=50).eoyFoodProduced(100)
    ) should be (156L)

    (testIsland(population=100,foodProductionFactor=200).eoyFoodProduced(100)
    ) should be (248L)

    intercept[java.lang.AssertionError] {
      testIsland().eoyFoodProduced(-1)
    }

    intercept[java.lang.AssertionError] {
      testIsland(population = -999).eoyFoodProduced(100)
    }

    (testIsland(foodProductionFactor = -999).eoyFoodProduced(100)
    ) should be (0L)
  }
  
  test("eoyRawMatProduced") {

    (testIsland().eoyRawMatProduced(workers=100)
    ) should be (100L)

    (testIsland(population=50,rawMatProductionFactor=50).eoyRawMatProduced(workers=100)
    ) should be (50L)

    (testIsland().eoyRawMatProduced(workers=0)
    ) should be (0L)

    (testIsland(population=0).eoyRawMatProduced(workers=100)
    ) should be (0L)

    (testIsland(population=100,rawMatProductionFactor=50).eoyRawMatProduced(workers=100)
    ) should be (78L)

    (testIsland(population=100,rawMatProductionFactor=200).eoyRawMatProduced(workers=100)
    ) should be (124L)

    intercept[java.lang.AssertionError] {
      testIsland().eoyRawMatProduced(workers= -1)
    }

    intercept[java.lang.AssertionError] {
      testIsland(population = -999).eoyRawMatProduced(workers=100)
    }

    (testIsland(rawMatProductionFactor = -999).eoyRawMatProduced(workers=100)
    ) should be (0L)
  }
  
  test("eoyGoodsProduced") {

    (testIsland().eoyGoodsProduced(workers=100)
    ) should be (100L)

    (testIsland(population=50,goodsProductionFactor=50).eoyGoodsProduced(workers=100)
    ) should be (50L)

    (testIsland(rawMatStored=50,goodsProductionFactor=50).eoyGoodsProduced(workers=100)
    ) should be (50L)

    (testIsland().eoyGoodsProduced(workers=0)
    ) should be (0L)

    (testIsland(population=0).eoyGoodsProduced(workers=100)
    ) should be (0L)

    (testIsland(rawMatStored=0).eoyGoodsProduced(workers=100)
    ) should be (0L)

    (testIsland(population=100,goodsProductionFactor=50).eoyGoodsProduced(workers=100)
    ) should be (78L)

    (testIsland(population=100,rawMatStored=999,goodsProductionFactor=200).eoyGoodsProduced(workers=100)
    ) should be (124L)

    intercept[java.lang.AssertionError] {
      testIsland().eoyGoodsProduced(workers= -1)
    }

    intercept[java.lang.AssertionError] {
      testIsland(population = -999).eoyGoodsProduced(workers=100)
    }

    intercept[java.lang.AssertionError] {
      testIsland(rawMatStored = -999).eoyGoodsProduced(workers=100)
    }

    (testIsland(goodsProductionFactor = -999).eoyGoodsProduced(workers=100)
    ) should be (0L)
  }
}
      
/* TODO

    def foodWillBeConsumed =
      math.min(foodStored, population)

    def goodsWillBeConsumed =
      math.min(goodsStored, population)


*/