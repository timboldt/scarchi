package boldt.scarchi

import scala.swing._
import java.awt.{ Color, Graphics2D }
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.Timer

object TestApp extends SimpleGUIApplication {

  var islands = for (i <- 1 to 500) yield new Island(mapWidth = 1000, mapHeight = 750, genome = Genome.random)
  
  def printBest {
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
    println(bestkey, bestval)
  }
  
  val mainPanel = new Panel {
    
    preferredSize = new Dimension(1000, 750)

    override def paint(g: Graphics2D) {
      islands = for (isle <- islands) yield new Island(isle, islands)
      printBest
      
      g.setPaint(Color.BLACK)
      g.fillRect(0, 0, size.width, size.height)
      for (isle <- islands) {
        val radiusP:Double = math.sqrt(isle.population)/10
        if (isle.happiness < 25) g.setPaint(Color.RED)
        else if (isle.happiness < 100) g.setPaint(Color.WHITE)
        else g.setPaint(Color.GREEN)
        
        g.fillOval((isle.x-radiusP).toInt, (isle.y-radiusP).toInt, (radiusP*2).toInt, (radiusP*2).toInt)
      }
      g.setPaint(Color.GRAY)
      for (isle1 <- islands) {
        for (isle2 <- islands) {
          if (Util.distanceBetweenPoints(isle1.x.toInt,isle1.y.toInt,isle2.x.toInt,isle2.y.toInt) < 100 && isle1.genome == isle2.genome)
            g.drawLine(isle1.x.toInt,isle1.y.toInt,isle2.x.toInt,isle2.y.toInt)
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