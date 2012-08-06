package boldt.scarchi

import scala.swing._
import java.awt.{ Color, Graphics2D }
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.Timer
import compat.Platform.currentTime

object TestApp extends SimpleGUIApplication {

  val mapHeight = 800
  val mapWidth = 1200
  val density:Double = 1.0/1500
  val circleScale:Double = 0.07
  
  var islands = for (i <- 1 to (density*mapHeight*mapWidth).toInt) yield new Island(mapWidth = mapWidth, mapHeight = mapHeight, genome = Genome.random)
  var overallBestKey = "(none)"
  var lastTime:Long = 0
  
  def calcBest = {
    val gs = for (i <- islands) yield Tuple(i.genome, i.score)
    val s = gs.groupBy ( _._1) .map { case (k,v) => k -> v.map(_._2).sum }
    
    var bestkey = ""
    var bestval = 0L
    for (k <- s.keys) {
      if (s(k) > bestval) {
        bestkey = k
        bestval = s(k)
      }
    }
    if (bestkey != overallBestKey || currentTime > lastTime + 10000) {
      val totalScore = gs.foldLeft(0L)(_ + _._2)
      println(bestkey, totalScore)
      overallBestKey = bestkey
      lastTime = currentTime
    }
    bestkey
  }
  
  val mainPanel = new Panel {
    
    preferredSize = new Dimension(mapWidth,mapHeight)

    override def paint(g: Graphics2D) {
      islands = for (isle <- islands) yield new Island(isle, islands)
      val bestkey = calcBest
      
      g.setPaint(Color.BLACK)
      g.fillRect(0, 0, size.width, size.height)
      for (isle <- islands) {
        val radiusP:Double = math.sqrt(isle.score)*circleScale
        val rgb = Genome.getRGB(isle.genome)
        g.setPaint(new Color(rgb._1, rgb._2, rgb._3))
        g.fillOval((isle.x-radiusP).toInt, (isle.y-radiusP).toInt, (radiusP*2).toInt, (radiusP*2).toInt)
      }
      for (isle1 <- islands) {
        for (isle2 <- islands) {
          if (Util.distanceBetweenPoints(isle1.x.toInt,isle1.y.toInt,isle2.x.toInt,isle2.y.toInt) < 100 && isle1.genome == isle2.genome) {
            val rgb = Genome.getRGB(isle1.genome)
            g.setPaint(new Color(rgb._1, rgb._2, rgb._3).brighter)
            g.drawLine(isle1.x.toInt,isle1.y.toInt,isle2.x.toInt,isle2.y.toInt)
          }
        }
      }
    }
  }

  def top = new MainFrame {
    title = "Test App"

    contents = new BorderPanel {
      layout += (mainPanel -> BorderPanel.Position.Center)
    }
  }
  
  val timer = new Timer(
    20,
    new ActionListener() {
      override def actionPerformed(e: ActionEvent) {
        mainPanel.repaint
      }
    }
  )

  timer.start
}