object Exercises {

  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal

  case class Shelter[+T <: Animal] (animals: List[T]) {

    def +[A >: T <: Animal](animalToAdd: A): Shelter[A] =
      Shelter[A](animals :+ animalToAdd)

    def ++[A >: T <: Animal](otherShelter: Shelter[A]): Shelter[A] =
      Shelter[A](animals ++ otherShelter.animals)

    def getNames: List[String] = {
      animals.map(animal => animal.name)
    }

    def feed[A >: T <: Animal](food: Food[A]): List[String] = {
      if (!food.isInstanceOf[Food[T]])
        throw new IllegalArgumentException
      var result = List[String]()
      for (animal <- animals) {
        result :+= s"${animal.name} eats ${food.name}"
      }
      result
    }
  }
    trait Food[T <: Animal] {
      def name: String
    }

    case object Meat extends Food[Animal] {
      override def name: String = "meat"
    }

    case object Milk extends Food[Animal] {
      override def name: String = "milk"
    }

    case object Bread extends Food[Dog] {
      override def name: String = "bread"
    }
}