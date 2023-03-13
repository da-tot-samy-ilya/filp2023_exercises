package exercises02

object Counter {

  private val re1    = "[()\\s.,!?:\\n\\t\\r]".r
  private val re2    = "[()\\s!?:\\n\\t\\r]".r
  private val letter = "[a-zA-Z]".r
  private val number = "\\d".r

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] =
    text
      .split(re1.regex)
      .filter(_.nonEmpty)
      .map(_.toLowerCase())
      .groupBy(identity)
      .map(t => (t._1, t._2.length))

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] =
    countWords(text).filter(item => letter.matches(item._1.slice(0, 1)))

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] =
    text
      .split(re2.regex)
      .filter(s => s.nonEmpty && number.matches(s.slice(0, 1)))
      .groupBy(identity)
      .map(t => (t._1, t._2.length))
}
