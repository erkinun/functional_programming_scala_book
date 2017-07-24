package com.asyaminor.functional.book.functional_state

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def runState(machine: Machine, inputs: List[Input]): State[Machine, (Int, Int)] = {
      inputs match {
        case x :: xs => x match {
          case Coin =>
            if (machine.candies > 0) runState(Machine(locked = false, machine.candies, machine.coins + 1), xs)
            else runState(Machine(machine.locked, machine.candies, machine.coins + 1), xs)
          case Turn =>
            if (!machine.locked) runState(Machine(locked = true, machine.candies - 1, machine.coins), xs)
            else runState(machine, xs)
        }
        case Nil => State(machine => ((machine.coins, machine.candies), machine))
      }
    }

    runState(this, inputs)
  }
}

