package textRank

abstract class Message
case class Count(nodeCount:Int, index: Int) extends Message
case class Go() extends Message
case class OneDown() extends Message
case class SendToken() extends Message
case class SendCount() extends Message
case class Hello() extends Message
case class Start() extends Message
