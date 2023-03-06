package exercises02

object Counter {

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] =
    text
      .split("[()\\s.,!?:\\n\\t\\r]")
      .filter(s => s.nonEmpty)
      .map(s => s.toLowerCase())
      .groupBy(l => l)
      .map(t => (t._1, t._2.length))

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] =
    countWords(text).filter(item => (97 to 122 contains item._1.charAt(0).toByte))

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] =
    text
      .split("[()\\s!?:\\n\\t\\r]")
      .filter(s => s.nonEmpty && Character.isDigit(s.charAt(0)))
      .groupBy(l => l)
      .map(t => (t._1, t._2.length))
}
