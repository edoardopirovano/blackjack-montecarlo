import scala.util.Random

/**
 * Class to represent a game being played.
 */
class Game {
  val deck = new Deck() // The deck being used.
  var dealerCards:List[Card] = List() // The dealer's cards.
  var playerCards:List[Card] = List() // The player's cards.
  var didDouble = false // Whether the player has doubled.
  var didStand = false // Whether the player has stood.

  /**
   * Start a new game from a random state.
   */
  def start():Unit = {
    didDouble = false
    didStand = false
    deck.addAll(playerCards ++ dealerCards)
    deck.shuffle()
    dealerCards = deck.getCard :: List()
    playerCards = List()
    var desiredValue = Random.nextInt(17) + 4
    if (desiredValue > 11 && Random.nextBoolean()) {
      desiredValue -= 11
      playerCards = deck.getAce :: playerCards
    }
    playerCards = deck.getValue(desiredValue) ++ playerCards
    if (!canPlay) start()
  }

  /**
   * @return Whether or not the player can play at the moment.
   */
  def canPlay = valueAndAces(playerCards)._1 < 21 && !didDouble && !didStand

  /**
   * Play an action.
   * @param action The action the player wants to play.
   */
  def play(action: Action.Value) = action match {
    case Action.Double =>
      playerCards = deck.getCard :: playerCards
      didDouble = true
    case Action.Hit =>
      playerCards = deck.getCard :: playerCards
    case Action.Stay =>
      didStand = true
  }

  /**
   * Finish the game by having the dealer play.
   * @return The reward for the given game (+1 for win, -1 for loss, 0 for draw, doubled in case of doubling).
   */
  def finishGame():Int = {
    while (shouldHit(dealerCards)) dealerCards = deck.getCard :: dealerCards
    val factor = if (didDouble) 2 else 1
    val (playerNum, _) = valueAndAces(playerCards)
    val (dealerNum, _) = valueAndAces(dealerCards)
    if (dealerNum <= 21 && playerNum < dealerNum || playerNum > 21) return -1 * factor
    if (playerNum <= 21 && (dealerNum < playerNum || dealerNum > 21)) return 1 * factor
    0
  }

  /**
   * Get the current state of the game.
   * @return The state the game is currently in.
   */
  def getState:State = {
    val (count, softAces) = valueAndAces(playerCards)
    new State(count, softAces, dealerCards.head)
  }

  /**
   * Return the value of a hand and whether or not in contains a usable ace.
   * @param cards The hand to consider.
   * @return A pair of the value of the cards and whether or not they contain a soft ace.
   */
  private def valueAndAces(cards:List[Card]):(Int, Boolean) = {
    var value = 0
    var aces = 0
    for (card <- cards) {
      value += card.value
      if (card.value == 1) {
        aces += 1
        value += 10
      }
    }
    while (value > 21 && aces > 0) {
      value -= 10
      aces -= 1
    }
    (value, aces != 0)
  }

  /**
   * Determines when the dealer hits.
   * @param cards The dealer's hand.
   * @return Whether or not the dealer hits by following his fixed policy (hit under 17 and soft 17).
   */
  private def shouldHit(cards:List[Card]): Boolean = {
    val (valueNum, usableAce) = valueAndAces(cards)
    valueNum <= 16 || (valueNum == 17 && usableAce)
  }
}
