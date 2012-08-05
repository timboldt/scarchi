package boldt.scarchi

object Island {


/*
(defn food-produced [workers {factor :food-production-factor population :population}]
    (assert (>= workers 0))
    (* 2 (diminished-return (min workers population) factor)))

(defn rawmat-produced [workers {factor :rawmat-production-factor population :population}]
    (assert (>= workers 0))
    (diminished-return (min workers population) factor))

(defn goods-produced [workers {factor :goods-production-factor population :population rawmat :rawmat-stored}]
    (assert (>= workers 0))
    (min rawmat (diminished-return (min workers population) factor)))

(defn food-consumed [{food :food-stored population :population}]
    (min food population))

(defn goods-consumed [{goods :goods-stored population :population}]
    (min goods population))

(defn calc-population [{food :food-stored population :population}]
    (if (<= population 0)
        0
        (let [
            random-jitter (rand-int 2)
            starvation (long (max (- (* 0.75 population) food) 0))
            births (long (* 0.03 population))
            deaths (long (* 0.02 population))
            ]
            (constrain-within
                (+ population random-jitter (- starvation) births (- deaths))
                0
                (* 2 population)
                ))))

(defn calc-happiness [{happiness :happiness food :food-stored goods :goods-stored population :population}]
    (if (<= population 0)
        0
        (let [
            food-quad (long (/ (* 4 food) population))
            goods-quad (long (/ (* 4 goods) population))
            food-delta (
                case food-quad
                    0 -50
                    1 -25
                    2 -10
                    3 0
                    4 10
                    20)
            goods-delta (
                case goods-quad
                    0 -20
                    1 -10
                    2 -5
                    3 0
                    4 5
                    10)
            ]
            (constrain-within (+ happiness food-delta goods-delta) 0 200)
            )))
*/
}

class Island(
  val population: Int,
  val happiness: Int,
  //
  val foodStored: Int,
  val rawMatStored: Int,
  val goodsStored: Int,
  //
  val foodProductionFactor: Int,
  val rawMatProductionFactor: Int,
  val goodsProductionFactor: Int,
  //
  val gene: String,
  val x: Double,
  val y: Double) {

  def this(population: Int, gene: String, x: Double, y: Double) {
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

  def this(gene: String, mapWidth: Int, mapHeight: Int) {
    this(
      population = (math.random * 500).toInt + 50,
      gene = gene,
      x = (math.random * mapWidth),
      y = (math.random * mapHeight))
  }

  def this(other: Island) {
    this(
      population = other.population, //TODO
      happiness = other.happiness, //TODO
      //
      foodStored = other.foodStored, //TODO
      rawMatStored = other.rawMatStored, //TODO
      goodsStored = other.goodsStored, //TODO
      //
      foodProductionFactor = other.foodProductionFactor,
      rawMatProductionFactor = other.rawMatProductionFactor,
      goodsProductionFactor = other.goodsProductionFactor,
      //
      gene = other.gene,
      x = other.x,
      y = other.y)
  }
}