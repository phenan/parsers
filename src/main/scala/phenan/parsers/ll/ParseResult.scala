package phenan.parsers.ll

/** 構文解析全体の結果を表現する。
  *
  * @tparam T 構文解析の結果の型
  */
sealed trait ParseResult [+T] {
  def message: String
}

/** 構文解析全体の成功を表現する。
  *
  * @param result 構文解析の結果の値
  * @tparam T 構文解析の結果の型
  */
case class ParseSuccess [+T] (result: T) extends ParseResult[T] {
  def message: String = "ParseSuccess : " + result
}

/** 構文解析全体の失敗を表現する。
  *
  * @param errors 構文解析エラーのリスト
  */
case class ParseFailure (errors: List[FailureMessage]) extends ParseResult[Nothing] {
  def message: String = errors.map(_.toString).mkString("ParseFailure\n", "\n", "")
}

/** 連接 p ~ q の構文解析結果を格納するためのデータ構造。
  *
  * パターンマッチ case a ~ b => ... のようにして値を取り出す。
  */
case class ~ [+A, +B] (first: A, second: B)