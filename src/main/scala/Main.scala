package boldt.scarchi

import scala.swing._
import java.awt.{ Color, Graphics2D }

object TestApp extends SimpleGUIApplication {

  val mainPanel = new Panel {
    
    preferredSize = new Dimension(500, 500)

    override def paint(g: Graphics2D) {
      val islands = for (i <- 1 to 200) yield new Island(mapHeight = size.height, mapWidth = size.width, gene = "ABCDABCDABCD")
      g.setPaint(Color.WHITE)
      g.fillRect(0, 0, size.width, size.height)
      g.setPaint(Color.BLACK)
      for (isle <- islands) {
        val radius = isle.population/100
        g.fillRect(isle.x.toInt-radius, isle.y.toInt-radius, radius*2, radius*2)
      }
      for (isle1 <- islands) {
        for (isle2 <- islands) {
          if (Util.distanceBetweenPoints(isle1.x.toInt,isle1.y.toInt,isle2.x.toInt,isle2.y.toInt) < 50)
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
}