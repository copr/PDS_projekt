package textRank

import akka.actor._

class Master(sentences: Array[Array[String]]) extends Actor {
  private var nodesFinished = 0
  val n = sentences.size
  var countsForSentences = new Array[Int](n)
  var nodes: Array[ActorRef] = new Array[ActorRef](n)
  val repeats = 5
  val numberOfTokens = n*repeats
  var tokensCounter = numberOfTokens


  def StartFunction = {
    (0 until n).foreach(index => nodes(index) = context.actorOf(Props(new Node(nodes, index, sentences, 0))))
      (0 until repeats).foreach(_ =>  nodes.map(x => x ! SendToken))


  }
  def receive = {
    case Start => StartFunction
    case OneDown => {
      tokensCounter -= 1
//      println("Master: " + tokensCounter)
      if (tokensCounter == 0) {
//        println("Master: vsechny tokeny se vratily")
        nodes.map(x => x ! SendCount)
      }

    }
    case Count(nodeCount: Int, index: Int) => {
      nodesFinished += 1
      countsForSentences(index) = nodeCount
//      println("Master: " + nodesFinished)
      if (nodesFinished == n) {
        val summ:Double = countsForSentences.foldLeft(0.0)((a,b) => a + b)
        val score = countsForSentences.map(x => x / summ)
        println()
        score.map(x => println(x))
        println(score.foldLeft(0.0)((a,b) => a + b))
        println("Master: vsechny nody skoncily")
        context.system.shutdown()
      }
    }
    case _ => {
      println("tohle by se nemelo stat")
    }
  }
}
