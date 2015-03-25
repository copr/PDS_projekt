package textRank

import akka.actor._

class Master(sentences: Array[Array[String]]) extends Actor {
  var tokensCounter = 0
  var nodesFinished = 0
  val n = sentences.size
  var countsForSentences = new Array[Int](n)
  var nodes: Array[ActorRef] = new Array[ActorRef](n)


  def StartFunction = {
    var i = 0
    for (i <- 0 until n) {
      nodes(i) = context.actorOf(Props(new Node(context.actorOf(Props(this)), nodes, i, sentences)))
      tokensCounter += 1
    }
    println("Na zacatku mame tokenu presne " + tokensCounter + " a n je " + n)
    nodes.map(x => x ! SendToken)
  }
  def receive = {
    case Start => StartFunction
    case OneDown => {
      tokensCounter -= 1
//      println(tokensCounter)
      if (tokensCounter == 0) {
        println("Master: jsme na nule u tokenu")
        nodes.map(x => x ! SendCount)
      }

    }
    case Count(nodeCount: Int, index: Int) => {
      context.stop(nodes(index))
//      println("Master: skoncil node " + index)
      countsForSentences(index) = nodeCount
      nodesFinished += 1
      if (nodesFinished == n) {
        println("mame vsechny")
        val summ:Double = countsForSentences.foldLeft(0.0)((a,b) => a + b)
        val score = countsForSentences.map(x => x / summ)
        score.map(x => println(x))
        System.exit(0)
      }
    }
    case Hello => {
      println("ahojky")
    }
  }
}
