package exercises02

object Counter {

  private val dividersForWords   = "[()\\s.,!?:\\n\\t\\r]".r
  private val dividersForNumbers = "[()\\s!?:\\n\\t\\r]".r
  private val letter             = "[a-zA-Z]".r
  private val number             = "\\d".r

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] =
    dividersForWords
      .split(text)
      .filter(_.nonEmpty)
      .map(_.toLowerCase())
      .groupBy(identity)
      .map(t => (t._1, t._2.length))

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] =
    countWords(text).filter(item => letter.matches(item._1.charAt(0).toString))

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] =
    dividersForNumbers
      .split(text)
      .filter(s => s.nonEmpty && number.matches(s.charAt(0).toString))
      .groupBy(identity)
      .map(t => (t._1, t._2.length))
}
