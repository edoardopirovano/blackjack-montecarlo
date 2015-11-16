import java.util.concurrent.CountDownLatch

import Action.Action

/**
 * A worker that runs Monte Carlo experiments.
 * @param policy The policy to follow and update.
 * @param latch A latch to declare termination for other threads waiting.
 */
class MCWorker(policy: Policy, latch: CountDownLatch) extends Runnable {
  val game = new Game()
  val numberOfExperiments = 10000000
  val epsilonReciprocal = 100

  override def run() = {
    for (_ <- 0 to numberOfExperiments) {
      game.start()
      var stateActions:List[(State, Action)] = List()
      while (game.canPlay) {
        val action = policy.epsilonSoft(game.getState, epsilonReciprocal)
        stateActions = (game.getState, action) :: stateActions
        game.play(action)
      }
      val reward = game.finishGame()
      for ((state, action) <- stateActions) policy.update(state, action, reward)
    }
    latch.countDown()
  }
}
