package homework_3

object Exercises extends App {
    def prettyBooleanFormatter1(x: Any): String = {
        if (x.getClass.getSimpleName == "Boolean")
            if (x.asInstanceOf[Boolean]) return "правда"
            else return "ложь"
        x.toString
    }

    def prettyBooleanFormatter2(x: Any): String = {
        if (x.isInstanceOf[Boolean])
            if (x.asInstanceOf[Boolean]) return "правда"
            else return "ложь"
        x.toString
    }

    def prettyBooleanFormatter3(x: Any): String = {
        x match {
            case a: Boolean => if (a) "правда" else "ложь"
            case _ => x.toString
        }
    }

    def max1(xs: Seq[Int]): Int = {
        if (xs.isEmpty)
            throw new IllegalArgumentException
        xs.max
    }

    def max2(xs: Seq[Int]): Seq[Int] = {
        if (xs.isEmpty)
            return Seq.empty
        Seq(xs.max)
    }

    def max3(xs: Seq[Int]): Option[Int] = {
        if (xs.isEmpty)
            return Option.empty
        Option(xs.max)
    }

    def sumIntegers[CollectionType <: Iterable[Int]](xs: CollectionType): Int = xs.sum

    def sum1(x: Int, y: Int): Int = sumIntegers(Iterable(x,y))
    def sum2(x: Int, y: Int): Int = sumIntegers(Seq(x,y))
    def sum3(x: Int, y: Int): Int = sumIntegers(IterableClass(x,y))

    case class IterableClass[T](x: T, y: T) extends Iterable[T] {
        override def iterator: Iterator[T] = Iterator(x, y)
    }
}