package phenan.parsers.reader

/**
  * Created by ichikawa on 2015/11/16.
  */
case class CharSequenceReader private (offset: Int, source: CharSeqSource) extends Reader[Char] {
  def atEnd: Boolean = source.atEnd(offset)
  def head: Char = source.at(offset)
  def tail: Reader[Char] = new CharSequenceReader(offset + 1, source)
  def pos: Position = new OffsetPosition(offset, source)
}

object CharSequenceReader {
  def apply (cs: CharSequence): CharSequenceReader = new CharSequenceReader(0, new CharSeqSource(cs))
}
