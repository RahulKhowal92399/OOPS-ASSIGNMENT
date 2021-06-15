trait Queue {
  var queue:List[Double] = List.empty
  var queue_start: Int = -1
  var queue_end: Int = -1

  def enqueue(item: Double): String =
  {
    if(queue_end == -1 && queue_start == -1)
    {
      queue_start = queue_start + 1
      queue_end = queue_end + 1
      queue = queue ::: List(item)
      "Enqueued"
    }
    else
    {
      queue_end = queue_end + 1
      queue = queue ::: List(item)
      "Enqueued"
    }
  }

  def dequeue(item: Int): String =
  {
    if(queue_start == -1 && queue_end == -1)
    {
      "Queue Underflow"
    }
    else if(queue_start == queue_end )
    {
      queue = queue.drop(item)
      queue_start = -1
      queue_end = -1
      "Dequeued"
    }
    else
    {
      queue = queue.drop(1)
      queue_start = queue_start + 1
      "Dequeued"
    }
  }
  def getQueue: List[Double] =
  {
    queue
  }
}

class DoubleQueue extends Queue
{
  override def enqueue(item: Double): String =
  {
    if(queue_end == -1 && queue_start == -1)
    {
      queue_start = queue_start + 1
      queue_end = queue_end + 1
      val ItemDouble = 2 * item
      queue = queue ::: List(ItemDouble)
      "Enqueued"
    }
    else
    {
      queue_end = queue_end + 1
      val ItemDouble = 2 * item
      queue = queue ::: List(ItemDouble)
      "Enqueued"
    }
  }
}
class SquareQueue extends Queue
{
  override def enqueue(item: Double): String =
  {
    if(queue_end == -1 && queue_start == -1)
    {
      queue_start = queue_start + 1
      queue_end = queue_end + 1
      val ItemSquare = item * item
      queue = queue ::: List(ItemSquare)
      "Enqueued"
    }
    else
    {
      queue_end = queue_end + 1
      val ItemSquare = item * item
      queue = queue ::: List(ItemSquare)
      "Enqueued"
    }
  }
}
object QueueObject extends App
{
  val Object1 = new DoubleQueue
  val Object2 = new SquareQueue
  println(Object1.enqueue(1))
  println(Object1.enqueue(2))
  println(Object1.enqueue(3))
  println(Object1.enqueue(4))
  println(Object1.getQueue)
  println(Object2.enqueue(5))
  println(Object2.enqueue(6))
  println(Object2.enqueue(7))
  println(Object2.enqueue(8))
  println(Object2.getQueue)
  println(Object2.dequeue(item = 4))
  println(Object2.getQueue)
  println(Object2.dequeue(item = 4))
  println(Object2.getQueue)
}
