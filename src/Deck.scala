import scala.util.Random

/**
 * Class to represent a card.
 * @param value The value of the card.
 */
case class Card(value: Int)

/**
 * Class to represent a deck of cards.
 */
class Deck {
  /**
   * Fill deck with suitable cards.
   */
  var cards:List[Card] = List()
  for (_ <- 0 to 3) {
    for (n <- 1 to 9) {
      cards = new Card(n) :: cards
    }
    for (_ <- 0 to 3) {
      cards = new Card(10) :: cards
    }
  }

  /**
   * Get an ace from the pack.
   * @return An ace from the pack.
   */
  def getAce: Card = {
    val (before, after) = cards.splitAt(cards.indexOf(Card(1)))
    cards = before ++ after.tail
    Card(1)
  }

  /**
   * Get cards adding up to a given value from the pack.
   * @param value The value to get cards adding up to.
   * @return Card adding up to the given value from the pack.
   */
  def getValue(value: Int) = {
    var result:List[Card] = List()
    var remain = value
    while (remain > 1) {
      val (before, after) = cards.splitAt(cards.indexWhere(_.value <= remain))
      remain -= after.head.value
      result = after.head :: result
      cards = before ++ after.tail
    }
    result
  }

  /**
   * Shuffle the deck.
   */
  def shuffle() = cards = Random.shuffle(cards)

  /**
   * Get a card from the top of the deck.
   * @return A card from the top of the deck.
   */
  def getCard: Card = {
    val card = cards.head
    cards = cards.tail
    card
  }

  /**
   * Add all given card to the deck.
   * @param newCards The cards to add.
   */
  def addAll(newCards:Seq[Card]) = cards = cards ++ newCards
}

