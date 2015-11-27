package phenan.parsers.reader

class OffsetPosition (val offset: Int, val source: CharSeqSource) extends Position {
  def lineString: String = source.lineString(offset)
  def column: Int = source.column(offset)
  def line: Int = source.line(offset)

  override def < (pos: Position): Boolean = pos match {
    case that: OffsetPosition => this.offset < that.offset
    case _ => super.< (pos)
  }

  override def <= (pos: Position): Boolean = pos match {
    case that: OffsetPosition => this.offset <= that.offset
    case _ => super.<= (pos)
  }

  override def equals (pos: Position): Boolean = pos match {
    case that: OffsetPosition => this.offset == that.offset && this.source == that.source
    case p => super.equals(p)
  }
}
