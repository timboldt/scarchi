package boldt.scarchi.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import boldt.scarchi.Genome

class GenomeObjectTest extends FunSuite with ShouldMatchers {

  val gAAAA = Vector(4,3,2,1)
  val gABCD = Vector(10,9,7,4)
  val gBBBB = Vector(8,6,4,2)
  val gDBCA = Vector(10,6,4,1)
  val gZZZZ = Vector(4*26,3*26,2*26,26)

  test("Genome.random") {
    val list = for (i <- 1 to 1000) yield Genome.random

    for (v <- list) {
      v should fullyMatch regex """[A-Z]{4}-[A-Z]{4}-[A-Z]{4}"""
    }
  }

  test("Genome.decodePart") {
    Genome.decodePart("AAAA") should equal (gAAAA)
    Genome.decodePart("BBBB") should equal (gBBBB)
    Genome.decodePart("ABCD") should equal (gABCD)
    Genome.decodePart("DBCA") should equal (gDBCA)
    Genome.decodePart("ZZZZ") should equal (gZZZZ)
  }

  test("Genome.decode") {
    intercept[java.lang.IndexOutOfBoundsException] {
      Genome.decode("")
    }
    intercept[java.lang.IndexOutOfBoundsException] {
      Genome.decode("A")
    }
    intercept[java.lang.IndexOutOfBoundsException] {
      Genome.decode("AAAA-A-AAAA")
    }

    //TODO: Question: Should we allow this?  Or must it have exactly 3*4 sets?
    Genome.decode("ABCD") should equal (Vector(gABCD))
    Genome.decode("ABCD-") should equal (Vector(gABCD))
    Genome.decode("ABCDE") should equal (Vector(gABCD))
    Genome.decode("ABCDE-ZZZZ") should equal (Vector(gABCD,gZZZZ))
    
    Genome.decode("AAAA-AAAA-AAAA") should equal (Vector(gAAAA,gAAAA,gAAAA))
    Genome.decode("ZZZZ-AAAA-BBBB") should equal (Vector(gZZZZ,gAAAA,gBBBB))
  }
}
