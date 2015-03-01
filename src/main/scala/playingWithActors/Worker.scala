package playingWithActors

import akka.actor._

case class SortThis(array: Array[Int])
 
class Worker extends Actor {
  def receive = {
    case SortThis(arr) =>
      sender ! arr
      context.stop(self)
  }
}
