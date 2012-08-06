package boldt.scarchi
import scala.util.Random

object Genome {
  
  private def randomChar: Char =
    (util.Random.nextInt(26) + 65).toChar
  
  // 3 strings of 4 uppercase letters, joined by dashes, e.g. "AAAA-BBBB-CCCC"
  def random: String =
    Seq.fill(3)(
      Seq.fill(4)(
        Genome.randomChar
      ).mkString
    ).mkString("-")

  def decodePart(g: String):Vector[Int] = {
    val v = g.map(_.toInt - 64)
    Vector(
      -1 * (v(0) + v(1)),
      -1 * v(1),
      v(2),
      v(2) + v(3)
    )
  }
    
  def decode(g: String):IndexedSeq[Vector[Int]] =
    g.split('-').toIndexedSeq.map(decodePart)
    
  def mutate(g: String, p: Double) = {
    if (math.random <= p) {
      val r = util.Random.nextInt(12)
      val pos = if (r >= 8) r + 2 else if (r >= 4) r + 1 else r
      g.substring(0, pos) + randomChar + g.substring(pos+1)
    }
    else {
      g
    }
  }
    
    
}
