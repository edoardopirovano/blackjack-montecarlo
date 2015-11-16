import Action.Action

import scala.util.Random
import scala.collection.concurrent.TrieMap

/**
 * Class to represent a policy.
 */
class Policy {
  /**
   * Create a mapping for storing estimated values and populate it with 0s.
   */
  var values: TrieMap[State, TrieMap[Action, (BigDecimal, Long)]] = TrieMap()
  for (x <- 3 to 20; softAce <- Seq(false, true); showingCard <- 1 to 10) {
    var map: TrieMap[Action, (BigDecimal, Long)] = TrieMap()
    Action.values foreach ((action:Action) => map += ((action, (0.0, 0))))
    values += ((new State(x, softAce, new Card(showingCard)), map))
  }

  /**
   * Output the current policy.
   */
  def output() = {
    for (softAce <- Seq(false, true); x <- 3 to 20; showingCard <- 1 to 10) {
      val state = new State(x, softAce, new Card(showingCard))
      println(state + " -> " + greedyPlay(state))
    }
  }

  /**
   * Choose a random action to play.
   * @return A random action.
   */
  def randomPlay: Action = Random.shuffle(Seq(Action.Double, Action.Hit, Action.Stay)).head

  /**
   * Choose the greedy action to play.
   * @param state The state to consider.
   * @return The current greedy action from the given state.
   */
  def greedyPlay(state: State): Action = values.getOrElse(state, null).maxBy(_._2._1)._1

  /**
   * Return a random action with probability 1/epsilonReciprocal or the greedy action otherwise.
   * @param state The state to consider.
   * @param epsilonReciprocal The reciprocal of epsilon that we want.
   * @return The action chosen.
   */
  def epsilonSoft(state: State, epsilonReciprocal: Int) = if (Random.nextInt(epsilonReciprocal) == 0) randomPlay else greedyPlay(state)

  /**
   * Update the policy by adding an observed reward for a given play.
   * @param state The state the play was made from.
   * @param action The action performed.
   * @param reward The reward obtained.
   */
  def update(state: State, action: Action, reward: Int) = {
    var map = values.getOrElse(state, null)
    val old = map.get(action).get
    map += ((action, (((old._1 * old._2) + reward) / (old._2 + 1), old._2 + 1)))
    values += ((state, map))
  }
}
