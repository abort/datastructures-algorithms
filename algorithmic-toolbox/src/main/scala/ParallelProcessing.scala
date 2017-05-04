import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.util.StringTokenizer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ParallelProcessing {
  object InputReader {
    val reader : BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    var tokenizer : StringTokenizer = null

    private def next() : String = {
      while (tokenizer == null || !tokenizer.hasMoreTokens) {
        tokenizer = new StringTokenizer(reader.readLine())
      }
      tokenizer.nextToken()
    }

    def nextInt() : Int = next().toInt
  }

  case class Worker(id : Int, var nextFreeTime : Long) extends Ordered[Worker] {
    override def compare(that: Worker) : Int = {
      val timeDifference = nextFreeTime - that.nextFreeTime
      if (timeDifference == 0) return id - that.id
      return timeDifference.toInt
    }
  }

  def main(args: Array[String]): Unit = {
    val out = new PrintWriter(new OutputStreamWriter(System.out))

    val threads = InputReader.nextInt
    val jobs = InputReader.nextInt
    val jobTimes = Array.ofDim[Long](jobs)
    for (i <- 0 until jobs) {
      jobTimes(i) = InputReader.nextInt()
    }

    val startingTimes = computeJobStartingTimes(threads, jobTimes)
    startingTimes.foreach { case (thread, time) => out.println(s"$thread $time") }
    out.close()
  }

  def computeJobStartingTimes(threads : Int, jobs : Array[Long]): Seq[(Int, Long)] = {
    val elements = (0 until threads).map(Worker(_, 0))
    val workerQueue = mutable.PriorityQueue(elements : _*)(Ordering[Worker].reverse)
    val outputQueue = ArrayBuffer[(Int, Long)]()

    for (i <- jobs.indices) {
      val worker = workerQueue.dequeue()
      outputQueue += ((worker.id, worker.nextFreeTime))
      worker.nextFreeTime += jobs(i)
      workerQueue += worker
    }

    outputQueue
  }
}