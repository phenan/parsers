package phenan.parsers.reader

trait Reader[T] {
  def atEnd: Boolean
  def head: T
  def tail: Reader[T]
  def pos: Position
}
