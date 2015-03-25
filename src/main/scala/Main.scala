import playingWithActors._
import akka.actor._
import textRank._

object Application {
  def main(args: Array[String]) = {
    val nameOfFile = "text"
    val sentences = scala.io.Source.fromFile(nameOfFile).mkString
    val result = TextRank.rank(sentences, 35)
    val system = ActorSystem("mySystem")
    val master = system.actorOf(Props(new Master(StringParser.wordsFromSentences(sentences))))
    result.map(x => println(x._3))
    master ! Start
    master ! Hello

//    println(result.foldLeft(0.0)((a,b) => a+b._3))


    val k = 15
  }
}
