package calculator

class StackableSignal[T](init: T) {
  var values: List[T] = List(init)

  def value: T = values.head

  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values

    try op finally values = values.tail
  }
}
