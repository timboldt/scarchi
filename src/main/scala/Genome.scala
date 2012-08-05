package boldt.scarchi
import scala.util.Random

object Genome {
  
  // 3 strings of 4 uppercase letters, joined by dashes, e.g. "AAAA-BBBB-CCCC"
  def random: String =
    Seq.fill(3)(
      Seq.fill(4)(
        (util.Random.nextInt(26) + 65).toChar
      ).mkString
    ).mkString("-")

  def decodePart(g: String):Vector[Int] = {
    val v = g.map(_.toInt - 64)
    Vector(
      v(0) + v(1) + v(2) + v(3),
      v(1) + v(2) + v(3),
      v(2) + v(3),
      v(3)
    )
  }
    
  def decode(g: String):IndexedSeq[Vector[Int]] =
    g.split('-').toIndexedSeq.map(decodePart)
}
