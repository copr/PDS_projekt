package textRank

object StringParser {
  def sentences(string: String): Array[String] = 
    string.split("[\n+\\.?!]")

  def words(string: String): Array[String] =
    string.split("[\\s+.!?,]").filterNot(_ == "")

  def wordsFromSentences(string: String): Array[Array[String]] = 
    sentences(string).map(x => words(x))
}
