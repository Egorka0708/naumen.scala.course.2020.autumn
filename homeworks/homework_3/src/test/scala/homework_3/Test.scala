import utest._

object Test extends TestSuite {
    val tests = Tests {
        'test_prettyBooleanFormatter1 - {
            assert(Exercises.prettyBooleanFormatter1("Hello") == "Hello")
            assert(Exercises.prettyBooleanFormatter1(12) == "12")
            assert(Exercises.prettyBooleanFormatter1(true) == "правда")
            assert(Exercises.prettyBooleanFormatter1(false) == "ложь")
        }

        'test_prettyBooleanFormatter2 - {
            assert(Exercises.prettyBooleanFormatter2("Hello") == "Hello")
            assert(Exercises.prettyBooleanFormatter2(12) == "12")
            assert(Exercises.prettyBooleanFormatter2(true) == "правда")
            assert(Exercises.prettyBooleanFormatter2(false) == "ложь")
        }

        'test_prettyBooleanFormatter3 - {
            assert(Exercises.prettyBooleanFormatter3("Hello") == "Hello")
            assert(Exercises.prettyBooleanFormatter3(12) == "12")
            assert(Exercises.prettyBooleanFormatter3(true) == "правда")
            assert(Exercises.prettyBooleanFormatter3(false) == "ложь")
        }

        'test_max1 - {
            assert(Exercises.max1(Seq(0)) == 0)
            assert(Exercises.max1(Seq(1,2,3)) == 3)
            assert(Exercises.max1(Seq(-1,-2,-3)) == -1)
            try Exercises.max1(Seq())
            catch { case e: Exception => print("Тест на пустую коллекцию пройден") }
        }

        'test_max2 - {
            assert(Exercises.max2(Seq(0)) == Seq(0))
            assert(Exercises.max2(Seq(1,2,3)) == Seq(3))
            assert(Exercises.max2(Seq(-1,-2,-3)) == Seq(-1))
            assert(Exercises.max2(Seq()) == Seq.empty)
        }

        'test_max3 - {
            assert(Exercises.max3(Seq(0)) == Option(0))
            assert(Exercises.max3(Seq(1,2,3)) == Option(3))
            assert(Exercises.max3(Seq(-1,-2,-3)) == Option(-1))
            assert(Exercises.max3(Seq()) == Option.empty)
        }

        'test_sum1 - {
            assert(Exercises.sum1(1,2) == 3)
            assert(Exercises.sum1(-2,1) == -1)
            assert(Exercises.sum1(0,0) == 0)
            assert(Exercises.sum1(-2,-3) == -5)
        }

        'test_sum2 - {
            assert(Exercises.sum2(1,2) == 3)
            assert(Exercises.sum2(-2,1) == -1)
            assert(Exercises.sum2(0,0) == 0)
            assert(Exercises.sum2(-2,-3) == -5)
        }

        'test_sum3 - {
            assert(Exercises.sum3(1,2) == 3)
            assert(Exercises.sum3(-2,1) == -1)
            assert(Exercises.sum3(0,0) == 0)
            assert(Exercises.sum3(-2,-3) == -5)
        }
    }
} 