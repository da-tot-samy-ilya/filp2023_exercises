package exercises03.game

object Game {
  def parseState(input: String, number: Int): State = {
    if (input == GameController.IGiveUp) GiveUp
    else
      input.toIntOption match {
        case Some(value) =>
          if (value == number) Guessed
          else if (value > number) NumberIsSmaller
          else NumberIsBigger
        case None => WrongInput
      }
  }

  def action(state: State, number: Int): GameController => Unit = state match {
    case GiveUp          => _.giveUp(number)
    case WrongInput      => _.wrongInput()
    case NumberIsBigger  => _.numberIsBigger()
    case NumberIsSmaller => _.numberIsSmaller()
    case Guessed         => _.guessed()
  }

  def completed(state: State): Boolean = state match {
    case Guessed => true
    case GiveUp  => true
    case _       => false
  }
}
