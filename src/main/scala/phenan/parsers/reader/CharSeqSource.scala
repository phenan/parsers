package phenan.parsers.reader

import scala.collection.mutable.ArrayBuffer

class CharSeqSource (source: CharSequence) {
  def at (offset: Int) = source.charAt(offset)

  def atEnd (offset: Int) = offset >= source.length

  def line (offset: Int) = line_bs(offset, 0, index.length - 1)

  def column (offset: Int) = offset - index(line(offset) - 1) + 1

  def lineString (offset: Int) = {
    val l = line(offset)
    source.subSequence(index(l - 1), index(l)).toString
  }

  private def line_bs (offset: Int, l: Int, h: Int): Int = {
    if (l + 1 < h) {
      val m = (l + h) / 2
      if (offset < index(m)) line_bs(offset, l, m)
      else line_bs(offset, m, h)
    }
    else l + 1
  }

  private lazy val index: Array[Int] = makeIndex(new ArrayBuffer[Int]() += 0, 1)

  private def makeIndex (ind: ArrayBuffer[Int], pos: Int): Array[Int] = {
    if (pos < source.length) {
      if (source.charAt(pos) == '\n') makeIndex(ind += (pos + 1), pos + 1)
      else makeIndex(ind, pos + 1)
    }
    else (ind += source.length).toArray
  }
}
