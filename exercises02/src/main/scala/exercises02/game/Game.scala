package exercises02.game

class Game(controller: GameController) {

  /**
    * Игра угадай число
    * Ввод и вывод необходимо осуществлять с помощью методов controller
    *
    * Игра должна вызывать controller.askNumber перед каждой попыткой игрока угадать число
    * И вызвать controller.nextLine для получения ввода игрока
    * Если игрок ввел число меньше загаданного, игра должна вызвать controller.numberIsBigger
    * Если игрок ввел число больше загаданного, игра должна вызвать controller.numberIsSmaller
    * Если игрок угадал число, игра должна закончиться и вызвать controller.guessed
    * Если игрок написал GameController.IGiveUp, игра должна закончиться и вызвать controller.giveUp(number)
    * Если игрок ввел неизвестную комбинацию символов, надо вызвать contoller.wrongInput и продолжить игру
    *
    * @param number загаданное число
    */
  def play(number: Int): Unit = {
    controller.askNumber()
    var input = controller.nextLine()
    while (true) {
      if (input == GameController.IGiveUp) {
        controller.giveUp(number)
        return
      }
      if (isDigit(input)) {
        val n = input.toInt
        if (n == number) {
          controller.guessed()
          return
        }
        else if (n > number) {
          controller.numberIsSmaller()
        }
        else {
          controller.numberIsBigger()
        }
      }
      else {
        controller.wrongInput()
      }
      controller.askNumber()
      input = controller.nextLine()
    }
  }
  def isDigit(n_s: String): Boolean = n_s.toCharArray.forall(c => Character.isDigit(c))
}
