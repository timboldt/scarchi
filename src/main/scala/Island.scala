package boldt.scarchi

object Island {
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
  val gene: String,
  val x: Double,
  val y: Double) {

  def this(population: Long, gene: String, x: Double, y: Double) {
    this(
      population = population,
      happiness = Util.randomPlusMinusPercent(100, 10),
      //
      foodStored = Util.randomPlusMinusPercent(population, 10),
      rawMatStored = Util.randomPlusMinusPercent(population, 10),
      goodsStored = Util.randomPlusMinusPercent(population, 10),
      //
      foodProductionFactor = Util.randomPlusMinusPercent(population, 30),
      rawMatProductionFactor = Util.randomPlusMinusPercent(population, 90),
      goodsProductionFactor = Util.randomPlusMinusPercent(population, 50),
      //
      gene = gene,
      x = x,
      y = y)
  }

  def this(gene: String, mapWidth: Long, mapHeight: Long) {
    this(
      population = (math.random * 500).toLong + 50,
      gene = gene,
      x = (math.random * mapWidth),
      y = (math.random * mapHeight))
  }

  def this(other: Island) {
    this(
      population = other.eoyPopulation,
      happiness = other.eoyHappiness,
      //
      foodStored = other.foodStored + other.eoyFoodProduced(other.population/3/*foodWorkers*/) - other.eoyFoodConsumed,
      rawMatStored = other.rawMatStored + other.eoyRawMatProduced(other.population/3/*workers*/) - other.eoyGoodsProduced(999/*workers*/),
      goodsStored = other.goodsStored + other.eoyGoodsProduced(other.population/3/*workers*/) - other.eoyGoodsConsumed,
      //
      foodProductionFactor = other.foodProductionFactor,
      rawMatProductionFactor = other.rawMatProductionFactor,
      goodsProductionFactor = other.goodsProductionFactor,
      //
      gene = other.gene,
      x = other.x,
      y = other.y)
  }

  def eoyPopulation = {
    val births:Long = (population * 3 / 100)
    val deaths:Long = (population * 2 / 100)
    val starvation:Long = math.max(0, (population * 75 / 100) + 1 - foodStored)
    val jitter:Long = (math.random * 2).toLong
    val rawPopulation = population + births - deaths - starvation + jitter
    math.max(0, rawPopulation)
  }
  
  def eoyHappiness = {
    val foodQuad:Long = (4 * foodStored) / population
    val foodDelta:Long = foodQuad match {
      case 0 => -50
      case 1 => -25
      case 2 => -10
      case 3 => 0
      case 4 => 10
      case _ => 20
    }
    val goodsQuad:Long = (4 * goodsStored) / population
    val goodsDelta:Long = goodsQuad match {
      case 0 => -20
      case 1 => -10
      case 2 => -5
      case 3 => 0
      case 4 => 5
      case _ => 10
    }
    val rawHappiness = (happiness + foodDelta + goodsDelta)//if (foodStored > population*75/100) 100 else (happiness + foodDelta + goodsDelta)
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

/*
                
                -----------------------------
                ; Genetics
                ;-----------------------------

                (defn- decode-genome-char [c]
                    (- (int c) 64)
                    )

                (defn- gene-values [[a b c d]]
                    (list (+ a b c d) (+ b c d) (+ c d) d)
                    )

                (defn decode-genome [gene]
                    (map #(gene-values (map decode-genome-char %)) (partition 4 gene))
                    )

                ;-----------------------------
                ; Genetic behavior
                ;-----------------------------

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

                (defn score [{happiness :happiness population :population}]
                    (+ happiness population)
                    )

                ;-----------------------------
                ; Year iteration
                ;-----------------------------


                (defn many-years [n island]
                    (if (> n 0)
                        (many-years (dec n) (next-year island))
                        island
                        ))
    */
}