import Exercises.{morse, wordReverse}

object Exercises {

  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse

  def fibonacci4Index(idx: Int): Int =
    if (idx == 0)
      0
    else if (idx == 1 || idx == 2)
      1
    else
      fibonacci4Index(idx - 1) + fibonacci4Index(idx - 2)

  def fibonacci(idx: Int): Seq[Int] =
    for (i <- 0 to idx)
      yield fibonacci4Index(i)

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String =
    text.toUpperCase.split("").map(symbol =>
      if (MORSE.contains(symbol))
        MORSE(symbol)
      else symbol).mkString(" ")

  def wordReverse(text: String): String =
    text
      .split("(?=[!. ,?])|(?<=[!. ,?])")
      .map(word => if (word.charAt(0).isUpper) {
        word.toLowerCase.reverse.capitalize
      } else
        word.reverse
      ).mkString("")
}

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println(wordReverse("Зима!.. Крестьянин, торжествуя..."))
    println(morse("Hello world!"))
  }
}