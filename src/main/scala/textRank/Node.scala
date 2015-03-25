package textRank

import akka.actor._

class Node(master: ActorRef, nodes: Array[ActorRef], index: Int, sentences: Array[Array[String]]) extends Actor {
  val r = scala.util.Random
  val d = 0.85
  val n = sentences.size
  var probabilities = new Array[Double](n+1)
  var count = 0
 

  def genPropabilities = {
    probabilities(0) = 0
    for (j <- 0 until n) {
      val outgoinWeights = sentences.foldLeft(0.0)((b, a) => b + TextRank.similarity(sentences(j), a))
      if (outgoinWeights == 0) {
        probabilities(j+1) = probabilities(j) + (1-d)/n
      } else {
      probabilities(j+1) = probabilities(j) + (1-d)/n + d*TextRank.similarity(sentences(j), sentences(index))/outgoinWeights
      }
    }
    probabilities(n) = 1
  }

  def receive = {
    case Go => {
      count = count + 1
      if (r.nextDouble < (1-d)) {
        master ! OneDown
      } else {
        sendToAnotherNode
      }
    }

    case SendCount => {
      master ! Count(count, index)
    }

    case SendToken => {
      genPropabilities
      count = count + 1
      if (r.nextDouble < (1-d)) {
        master ! OneDown
      } else {
        sendToAnotherNode
      }
    }
  }

  def sendToAnotherNode = {
    val randomDouble = r.nextDouble()
    var i = 0
    for(i <- 0 until n) {
      if (probabilities(i) < randomDouble && probabilities(i+1) > randomDouble) {
        nodes(i) ! Go
      }
    }
  }

  
}


