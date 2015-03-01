import playingWithActors._
import akka.actor._
import textRank.TextRank


object Application {
  def main(args: Array[String]) = {
    val sentences = scala.io.Source.fromFile("text").mkString
    val result = TextRank.rank(sentences, 15)
    result.map(x => println(x._1))
    val k = 5
  }
}
