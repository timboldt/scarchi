package boldt.scarchi

object Island {
  val FOOD = 0;
  val RAWMAT = 1;
  val GOODS = 2;
  val MUTATION_RATE = 0.001 //0.0003
  
  def deriveGene(isle:Island, islands:IndexedSeq[Island]):String = {
    var g = isle.genome
    
    for (i <- islands) {
      
      val p = math.random * (isle.population/(i.population+1.0) + 1.0)
      
      val d = Util.distanceBetweenPoints(isle.x, isle.y, i.x, i.y)
      
      if (d > 0.0) {
        if (
          (d <= 1.0 && 1.0 > p) ||
          (d > 1.0 && (5.0 / (d*d)) > p)
        ) {
          //println(isle.population, i.population, d, p)
          g = i.genome
        }
      }
    }
        
    Genome.mutate(g, Island.MUTATION_RATE/(isle.happiness+1.0))
  }
}

class Island(
  val population: Long,
  val happiness: Long,
  //
  val foodStored: Long,
  val rawMatStored: Long,
  val goodsStored: Long,
  //
  val foodProductionFactor: Long,
  val rawMatProductionFactor: Long,
  val goodsProductionFactor: Long,
  //
  val genome: String,
  val x: Double,
  val y: Double) {

  def this(population: Long, genome: String, x: Double, y: Double) {
    this(
      population = population,
      happiness = Util.randomPlusMinusPercent(100, 10),
      //
      foodStored = Util.randomPlusMinusPercent(population, 10),
      rawMatStored = Util.randomPlusMinusPercent(population, 10),
      goodsStored = Util.randomPlusMinusPercent(population, 10),
      //
      foodProductionFactor = Util.randomPlusMinusPercent(population, 90),
      rawMatProductionFactor = Util.randomPlusMinusPercent(population, 90),
      goodsProductionFactor = Util.randomPlusMinusPercent(population, 90),
      //
      genome = genome,
      x = x,
      y = y)
  }

  def this(genome: String, mapWidth: Long, mapHeight: Long) {
    this(
      population = (math.random * 5000).toLong + 50,
      genome = genome,
      x = (math.random * mapWidth),
      y = (math.random * mapHeight))
  }

  def this(other: Island, islands: IndexedSeq[Island]) {
    this(
      population = other.eoyPopulation,
      happiness = other.eoyHappiness,
      //
      foodStored = other.foodStored + other.eoyFoodProduced(other.workerAllocation(Island.FOOD)) - other.eoyFoodConsumed,
      rawMatStored = other.rawMatStored + other.eoyRawMatProduced(other.workerAllocation(Island.RAWMAT)) - other.eoyGoodsProduced(other.workerAllocation(Island.GOODS)),
      goodsStored = other.goodsStored + other.eoyGoodsProduced(other.workerAllocation(Island.GOODS)) - other.eoyGoodsConsumed,
      //
      foodProductionFactor = other.foodProductionFactor,
      rawMatProductionFactor = other.rawMatProductionFactor,
      goodsProductionFactor = other.goodsProductionFactor,
      //
      genome = Island.deriveGene(other, islands),
      x = other.x,
      y = other.y)
  }

  def eoyPopulation = {
    val births:Long =
      if (happiness >=150) population * 3 / 100
      else if (happiness >=50) population * 2 / 100
      else population * 1 / 100
    val deaths:Long = (population * 2 / 100)
    val starvation:Long = math.max(0, (population * 75 / 100) + 1 - foodStored)
    val jitter:Long = (math.random * 2).toLong
    val rawPopulation = population + births - deaths - starvation + jitter
    math.max(1, rawPopulation)
  }

  private def resourceScore(r:Long, g:Vector[Int]):Long = {
    val quad:Int = math.max(0, math.min(3, (2 * r) / population)).toInt
    g(quad)
  }
    
  def eoyHappiness = {
    val g = Genome.decode(genome)
    val rawHappiness = happiness + resourceScore(foodStored, g(Island.FOOD)) + resourceScore(goodsStored, g(Island.GOODS))
    //if (foodStored > population*75/100) 100 else (happiness + foodDelta + goodsDelta)
    math.min(200, math.max(0, rawHappiness))
  }
              
  def eoyFoodProduced(workers:Long):Long = {
    assert(population >= 0)
    assert(workers >= 0)
    Util.diminishedReturn(
      math.min(workers, population),
      foodProductionFactor
    ) * 2
  }

  def eoyRawMatProduced(workers:Long):Long = {
    assert(population >= 0)
    assert(workers >= 0)
    Util.diminishedReturn(
      math.min(workers, population),
      rawMatProductionFactor
    )
  }

  def eoyGoodsProduced(workers:Long):Long = {
    assert(population >= 0)
    assert(workers >= 0)
    assert(rawMatStored >= 0)
    math.min(
      Util.diminishedReturn(
        math.min(workers, population),
        goodsProductionFactor
      ),
      rawMatStored
    )
  }
  
  def eoyFoodConsumed =
    math.min(foodStored, population)
    
  def eoyGoodsConsumed =
    math.min(goodsStored, population)

  /*******************
    Genetic behavior
  *******************/
  private def estimatedValueOfWorkerAllocation(workers:(Long,Long,Long)):Double = {
    val g = Genome.decode(genome)
    
    resourceScore(Util.diminishedReturn(workers._1, foodProductionFactor) * 2 + foodStored, g(Island.FOOD)) +
    resourceScore(Util.diminishedReturn(workers._2, rawMatProductionFactor) + rawMatStored, g(Island.RAWMAT)) +
    resourceScore(Util.diminishedReturn(workers._3, goodsProductionFactor) + goodsStored, g(Island.GOODS))
  }

  private def getBestAllocation(workers:Vector[(Long,Long,Long)]):(Long,Long,Long) = {
    
    //TODO: learn to do this the functional way
    
    var best = (0L,0L,0L)
    var bestVal = -99999.0
    
    for (w <- workers) {
      val v = estimatedValueOfWorkerAllocation(w)
      if (v > bestVal) {
        best = w
        bestVal = v
      }
    }
    best
  }
  
  private def calcWorkerAllocation(workers:(Long,Long,Long) = Tuple(0,0,0)):Vector[Long] = {
    val increments:Long = 10
    val popIncrement:Long = population / increments
    val maxWorkers = popIncrement * increments
    
    if ((workers._1 + workers._2 + workers._3) >= maxWorkers)
      Vector(workers._1, workers._2, workers._3)
    else {
      val best = getBestAllocation(
        Vector(
          Tuple(workers._1 + popIncrement, workers._2, workers._3),
          Tuple(workers._1, workers._2 + popIncrement, workers._3),
          Tuple(workers._1, workers._2, workers._3 + popIncrement)
        )
      )
      calcWorkerAllocation(best)
    }
  }
  
  val workerAllocation:Vector[Long] =
    calcWorkerAllocation()

/*
; trade first?  or allocate workers first? or both?
(defn worker-allocation [island]
    (let [
        pop (:population island)
        storage [(:food-stored island) (:rawmat-stored island) (:goods-stored island)]
        value-table (decode-genome (:gene island))
        ]
    (loop [
        workers [0 0 0]
        ]
        (let [
            pop-10th (long (/ pop 10))
            max-workers (* 10 pop-10th)
            effective-storage (vec (map + storage workers))
            quadrants (map #(pop-quad % pop) effective-storage)
            valuations (map #(nth %1 (dec %2)) value-table quadrants)
            best-value (reduce max valuations)
            ]
            (if
                (>= (reduce + workers) max-workers )
                workers
                (cond
                    ; Very ugly: need to learn more Clojure secrets...
                    (= best-value (nth valuations 0)) (recur (vector (+ pop-10th (nth workers 0)) (nth workers 1) (nth workers 2)))
                    (= best-value (nth valuations 1)) (recur (vector (nth workers 0) (+ pop-10th (nth workers 1)) (nth workers 2)))
                    :else (recur (vector (nth workers 0) (nth workers 1) (+ pop-10th (nth workers 2))))
                    ))))))
*/

  def score:Long =
    happiness + population
}