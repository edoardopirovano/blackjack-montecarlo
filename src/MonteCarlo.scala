import java.util.concurrent.CountDownLatch

/**
 * Main object to run one or more Monte Carlo workers.
 */
object MonteCarlo {
  val numberOfWorkers = 10

  /**
   * Run the program
   * @param args No arguments are taken.
   */
  def main(args: Array[String]) {
    val policy = new Policy()
    val latch = new CountDownLatch(numberOfWorkers)
    for (_ <- 0 until numberOfWorkers) {
      val thread = new Thread(new MCWorker(policy, latch))
      thread.start()
    }
    latch.await()
    policy.output()
  }
}
