import utest._
import Exercises.Vector2D

object Test extends TestSuite {
  val ballsTest1: Map[String, (Int, Double)] = Map(
    "Gold" -> (2,   19.32)
  )
  val ballsTest2: Map[String, (Int, Double)] = Map(
    "Gold" -> (2,   19.32), "Silver" -> (4,   4.505)
  )
  val ballsTest3: Map[String, (Int, Double)] = Map(
      "Aluminum" -> (3,   2.6889), "Tungsten" ->  (2,   19.35), "Calcium" ->   (8,   1.55),
      "Gold" ->     (2,   19.32),  "Potassium" -> (14,  0.862),  "Cobalt" ->    (4,   8.90),
      "Lithium" ->  (12,  0.534), "Iron" ->      (3,   7.874), "Chrome" ->   (3,   7.18),
      "Graphite" ->  (12,  2.1), "Magnesium" -> (10,  1.738)
    )
  val ballsTest4: Map[String, (Int, Double)] = Map(
      "Tungsten" -> (5, 19.35),  "Aluminum" ->  (12, 2.6889), "Graphite" ->  (4, 2.1),   "Iron" ->      (1, 7.874),
      "Gold" ->     (1, 19.32),  "Potassium" -> (12, 0.862), "Cobalt" ->    (13, 8.90),   "Copper" ->    (2, 8.96),
      "Calcium" ->  (3, 1.55),   "Lithium" ->   (13, 0.534), "Magnesium" -> (7, 1.738), "Sodium" ->    (1, 0.971),
      "Nickel" ->   (4, 8.91),   "Tin" ->       (10, 7.29),   "Platinum" ->  (2, 21.45),  "Plutonium" -> (5, 19.25),
      "Lead" ->     (1, 11.336), "Titanium" ->  (6, 10.50),  "Silver" ->    (1, 4.505),  "Uranium" ->   (3, 19.04),
      "Chrome" ->   (3, 7.18),   "Cesium" ->    (7, 1.873),  "Zirconium" -> (2, 6.45)
    )

  val tests = Tests{
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(0, 1) == 0)
      assert(Exercises.sumOfDivBy3Or5(0, 3) == 3)
      assert(Exercises.sumOfDivBy3Or5(-5, 0) == -8)
      assert(Exercises.sumOfDivBy3Or5(-5, 9) == 15)
      assert(Exercises.sumOfDivBy3Or5(0, 30) == 225)
    }
    'test_primeFactor - {
      assert(Exercises.primeFactor(1) == Seq())
      assert(Exercises.primeFactor(2) == Seq(2))
      assert(Exercises.primeFactor(6) == Seq(2,3))
      assert(Exercises.primeFactor(64) == Seq(2))
      assert(Exercises.primeFactor(100) == Seq(2, 5))
      assert(Exercises.primeFactor(121) == Seq(11))
      assert(Exercises.primeFactor(997) == Seq(997))
      assert(Exercises.primeFactor(43868) == Seq(2, 11, 997))
    }
    'test_sumScalars - {
      assert(Exercises.sumScalars(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0)) == 16)
      assert(Exercises.sumScalars(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(3.0, 2.0), Vector2D(3.0, 2.0)) == 21)
      assert(Exercises.sumScalars(Vector2D(3.0, 5.0), Vector2D(1.0, 3.0), Vector2D(4.0, 10.0), Vector2D(3.0, 5.0)) == 80)
      assert(Exercises.sumScalars(Vector2D(0.0, 0.0), Vector2D(0.0, 0.0), Vector2D(0.0, 0.0), Vector2D(0.0, 0.0)) == 0)
      assert(Exercises.sumScalars(Vector2D(2.145, 2.234), Vector2D(2.228, 2.123), Vector2D(2.199, 2.897), Vector2D(2.77777, 2.98789)) == 24.286075559999997)
      assert(Exercises.sumScalars(Vector2D(-2.0, -3.0), Vector2D(-4.0, -5.123), Vector2D(-2.0, -1.124), Vector2D(-22.3, 2.0)) == 65.721)
    }
    'test_sumCosines - {
      assert(Exercises.sumCosines(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(2.0, 2.0)) == 1.9999999999999998)
      assert(Exercises.sumCosines(Vector2D(2.0, 2.0), Vector2D(2.0, 2.0), Vector2D(3.0, 2.0), Vector2D(3.0, 2.0)) == 2.0)
      assert(Exercises.sumCosines(Vector2D(3.0, 5.0), Vector2D(1.0, 3.0), Vector2D(4.0, 10.0), Vector2D(3.0, 5.0)) == 1.9634281808965999)
      assert(Exercises.sumCosines(Vector2D(2.145, 2.234), Vector2D(2.228, 2.123), Vector2D(2.199, 2.897), Vector2D(2.77777, 2.98789)) == 1.9940467771497488)
      assert(Exercises.sumCosines(Vector2D(-2.0, -3.0), Vector2D(-4.0, -5.123), Vector2D(-2.0, -1.124), Vector2D(-22.3, 2.0)) == 1.821707518082242)
    }
    'test_sortByHeavyweight - {
      assert(Exercises.sortByHeavyweight(ballsTest1) == Seq("Gold"))
      assert(Exercises.sortByHeavyweight(ballsTest2) == Seq("Gold", "Silver"))
      assert(Exercises.sortByHeavyweight(ballsTest3) == Seq("Aluminum", "Gold", "Tungsten", "Chrome", "Iron", "Cobalt", "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"))
      assert(Exercises.sortByHeavyweight(ballsTest4) == Seq("Sodium", "Silver", "Iron", "Lead", "Gold", "Calcium", "Zirconium", "Copper", "Graphite", "Platinum", "Chrome", "Uranium", "Nickel", "Magnesium", "Cesium", "Lithium", "Potassium", "Titanium", "Plutonium", "Tungsten", "Aluminum", "Tin", "Cobalt"))
      assert(Exercises.sortByHeavyweight(Map()) == Seq())
    }
  }
}
