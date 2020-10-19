object Task extends App {
  val name = "Egor"
  println(f"Hello Scala, this is $name")

  val greetings = Array("Hello", "Hola", "Guten tag")

  for (greet <- greetings)
    println(f"$greet Scala, this is $name")

  val reverseName = name.reverse
  for (greet <- greetings)
    println(f"$greet Scala, this is $reverseName")
}
