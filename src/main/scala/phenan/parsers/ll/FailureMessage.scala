package phenan.parsers.ll

import phenan.parsers.reader._

/** 構文解析エラーのメッセージを表現する。
  *
  * @param expected 期待していた入力の種類
  * @param found 実際の入力
  * @param pos エラー発生位置
  */
case class FailureMessage (expected: String, found: String, pos: Position) {
  override def toString = {
    "[" + pos.line + ", " + pos.column + "] expected " + expected + ", but found " + found + "\n> " + pos.lineString + "\n> " + (1 until pos.column).map(_ => " ").mkString + "^"
  }
}

/** 構文解析のエラーメッセージの雛形を表現する。
  *
  * @tparam E 構文解析の入力列の要素の型
  */
case class FailureBuilder[E] (expected: String, find: Reader[E] => String, priority: Int) {
  def build (in: Reader[E]): FailureMessage = FailureMessage(expected, find(in), in.pos)

  def < (that: FailureBuilder[E]): Boolean = priority < that.priority
  def > (that: FailureBuilder[E]) = priority > that.priority
  def <= (that: FailureBuilder[E]): Boolean = priority <= that.priority
  def >= (that: FailureBuilder[E]): Boolean = priority >= that.priority
}
