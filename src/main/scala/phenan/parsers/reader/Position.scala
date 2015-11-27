package phenan.parsers.reader

trait Position {
  def lineString: String
  def column: Int
  def line: Int

  def < (that: Position): Boolean = {
    this.line < that.line || ( this.line == that.line && this.column < that.column )
  }

  def <= (that: Position): Boolean = {
    this.line <= that.line || ( this.line == that.line && this.column <= that.column )
  }

  def > (that: Position): Boolean = that < this

  def >= (that: Position): Boolean = that <= this

  override def equals (obj: Any): Boolean = obj match {
    case that: Position => equals(that)
    case _ => false
  }

  def equals (that: Position): Boolean = this.line == that.line && this.column == that.column
}

object Position {
  implicit val ordering: Ordering[Position] = new Ordering[Position] {
    def compare(x: Position, y: Position): Int = {
      if (x < y) -1
      else if (x > y) 1
      else 0
    }
  }
}
