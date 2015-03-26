package textRank

import akka.actor._

class Node(nodes: Array[ActorRef], index: Int, sentences: Array[Array[String]]) extends Actor {
  val r = scala.util.Random
  val d = 0.85
  val n = sentences.size
  private var master: Option[ActorRef] = None
  private var probabilities = new Array[Double](n+1)
  private var count = 0
 
  def this(nodes: Array[ActorRef], index: Int, sentences: Array[Array[String]], dummy: Int) = {
    this(nodes, index, sentences)
    genPropabilities
  }


  def genPropabilities = {
    probabilities(0) = 0
    for (j <- 0 until n) {
      val outgoinWeights = sentences.foldLeft(0.0)((b, a) => b + TextRank.similarity(sentences(j), a))
      if (outgoinWeights == 0) {
        probabilities(j+1) = probabilities(j) + (1-d)/n
      } else {
        probabilities(j+1) = probabilities(j) + (1-d)/n 
        + d*TextRank.similarity(sentences(j), sentences(index))/outgoinWeights
      }
    }
    probabilities(n) = 1
  }

  def receive = {
    case Go => {
      count = count + 1
      if (r.nextFloat < (1-d)) {
        master.map(_ ! OneDown)
      } else {
        sendToAnotherNode
      }
    }

    case SendToken => {
      master = Some(sender)
      self ! Go
    }

    case SendCount => {
      sender ! Count(count, index)
      context.stop(self)
    }
  }

  def sendToAnotherNode = {
    val randomDouble = r.nextFloat()
    var i = 0
    var k = false
    for(i <- 0 until n) {
      if (probabilities(i) < randomDouble && probabilities(i+1) > randomDouble) {
        nodes(i) ! Go
        k = true
      } 
    }
//    if (!k) {
//      println("Node: token se nevlez do pravdepodobnosti s pravdepodobnosti " + randomDouble )
//g    }
  }

  
}


