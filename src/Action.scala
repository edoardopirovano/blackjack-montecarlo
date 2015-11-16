/**
 * Represents a state that the game can be in.
 * @param value The current value of the player's cards.
 * @param softAce Whether or not the player has a usable ace.
 * @param showingCard The card the dealer has showing.
 */
case class State(value: Int, softAce: Boolean, showingCard: Card)

/**
 * An object to represent the possible actions of Hit, Stay and Double.
 */
object Action extends Enumeration {
  type Action = Value
  val Double, Hit, Stay = Value
}