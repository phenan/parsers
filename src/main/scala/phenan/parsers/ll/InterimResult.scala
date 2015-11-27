package phenan.parsers.ll

/**
  * Created by ichikawa on 2015/11/18.
  */

import phenan.parsers.reader.Reader

/** 構文解析の途中の結果を表現する。
  *
  * @tparam E 入力列の要素の型
  * @tparam T 途中結果の型
  */
private[ll] sealed trait InterimResult[E, +T] {
  def next: Reader[E]
  def bestFailure: InterimFailureInfo[E]
  def map [U] (f: T => U): InterimResult[E, U]
  def seq [U] (f: Reader[E] => InterimResult[E, U]): InterimResult[E, T ~ U]
  def orElse [U >: T] (r: => InterimResult[E, U]): InterimResult[E, U]
  def longest [U >: T] (r: => InterimResult[E, U]): InterimResult[E, U] = {
    val that = r
    if (this.isBetterThan(that)) this.withFailInfo(that.bestFailure)
    else that.withFailInfo(this.bestFailure)
  }

  def panic [U] (f: Reader[E] => InterimResult[E, U]): InterimResult[E, T]
  def catches (f: FailureBuilder[E] => FailureBuilder[E]): InterimResult[E, T]

  def withFailInfo (fail: InterimFailureInfo[E]): InterimResult[E, T]
  def isBetterThan [U >: T] (that: InterimResult[E, U]): Boolean
}

/** 成功した構文解析の結果を表現する。
  *
  * @param result 構文解析結果の値
  * @param next 構文解析の残りの入力
  * @param bestFailure ここまでの構文解析の失敗のうち最良のもの
  * @tparam E 入力列の要素の型
  * @tparam T 構文解析結果の型
  */
private[ll] case class InterimSuccess[E, +T](result: T, next: Reader[E], bestFailure: InterimFailureInfo[E]) extends InterimResult[E, T] {
  def map [U] (f: T => U): InterimResult[E, U] = InterimSuccess(f(result), next, bestFailure)
  def seq [U] (f: Reader[E] => InterimResult[E, U]): InterimResult[E, T ~ U] = f(next) match {
    case InterimSuccess(u, n, bf) => InterimSuccess(new ~ (result, u), n, InterimFailureInfo.best(bestFailure, bf))
    case s: PseudoSuccess[E]      => s.withFailInfo(bestFailure)
    case f: InterimFailure[E]     => f.withFailInfo(bestFailure)
  }
  def orElse [U >: T] (r: => InterimResult[E, U]): InterimResult[E, U] = this

  def panic [U] (f: Reader[E] => InterimResult[E, U]): InterimResult[E, T] = this
  def catches (f: FailureBuilder[E] => FailureBuilder[E]): InterimResult[E, T] = this

  def withFailInfo (fail: InterimFailureInfo[E]): InterimResult[E, T] = InterimSuccess(result, next, InterimFailureInfo.best(bestFailure, fail))
  def isBetterThan [U >: T] (r: InterimResult[E, U]): Boolean = r match {
    case that: InterimSuccess[E, U] => this.next.pos > that.next.pos
    case _ => true
  }
}

/** 恐慌モードによりエラー回復した後の構文解析の成功を表現する。
  *
  * @param errors 回復済みのエラーのリスト
  * @param next 構文解析の残りの入力
  * @param bestFailure ここまでの構文解析の失敗のうち最良のもの
  * @tparam E 入力列の要素の型
  */
private[ll] case class PseudoSuccess[E](errors: List[InterimFailureInfo[E]], next: Reader[E], bestFailure: InterimFailureInfo[E]) extends InterimResult[E, Nothing] {
  def map [U] (f: Nothing => U): InterimResult[E, U] = this
  def seq [U] (f: Reader[E] => InterimResult[E, U]): InterimResult[E, Nothing ~ U] = f(next) match {
    case InterimSuccess(u, n, bf) => PseudoSuccess(errors, n, InterimFailureInfo.best(bestFailure, bf))
    case PseudoSuccess(es, n, bf) => PseudoSuccess(errors ++ es, n, InterimFailureInfo.best(bestFailure, bf))
    case InterimFailure(fail, es) => InterimFailure(InterimFailureInfo.best(bestFailure, fail), errors ++ es)
  }
  def orElse [U] (r: => InterimResult[E, U]): InterimResult[E, U] = longest(r)

  def panic [U] (f: Reader[E] => InterimResult[E, U]): InterimResult[E, Nothing] = this
  def catches (f: FailureBuilder[E] => FailureBuilder[E]): InterimResult[E, Nothing] = this

  def withFailInfo (fail: InterimFailureInfo[E]): InterimResult[E, Nothing] = PseudoSuccess(errors, next, InterimFailureInfo.best(bestFailure, fail))

  def isBetterThan [U] (r: InterimResult[E, U]): Boolean = r match {
    case that: InterimSuccess[E, U] => this.next.pos > that.next.pos
    case that: PseudoSuccess[E]     => this.next.pos > that.next.pos
    case _: InterimFailure[E]       => true
  }
}

/** 構文解析の失敗を表現する。
  *
  * @param fail 構文解析のエラー情報
  * @param errors 回復済みのエラーのリスト
  * @tparam E 入力列の要素の型
  */
private[ll] case class InterimFailure[E](fail: InterimFailureInfo[E], errors: List[InterimFailureInfo[E]]) extends InterimResult[E, Nothing] {
  def next = fail.next
  def bestFailure = fail

  def map [U] (f: Nothing => U): InterimResult[E, U] = this
  def seq [U] (f: Reader[E] => InterimResult[E, U]): InterimResult[E, Nothing ~ U] = this
  def orElse [U] (r: => InterimResult[E, U]): InterimResult[E, U] = longest(r)

  def panic [U] (f: Reader[E] => InterimResult[E, U]): InterimResult[E, Nothing] = f(next) match {
    case InterimSuccess(_, n, _) => PseudoSuccess(fail :: errors, n, fail)
    case _ => this
  }
  def catches (f: FailureBuilder[E] => FailureBuilder[E]): InterimResult[E, Nothing] = InterimFailure(InterimFailureInfo(f(fail.failBuilder), fail.next), errors)

  def withFailInfo (fail: InterimFailureInfo[E]): InterimResult[E, Nothing] = InterimFailure(InterimFailureInfo.best(bestFailure, fail), errors)

  def isBetterThan [U] (r: InterimResult[E, U]): Boolean = r match {
    case that: InterimFailure[E] => this.next.pos > that.next.pos
    case _                       => false
  }
}

/** 構文解析のエラー情報を保持する。
  *
  * @param failBuilder エラーメッセージの雛形
  * @param next 構文解析が失敗した部分以降の入力
  * @tparam E 入力列の要素の型
  */
private[ll] case class InterimFailureInfo [E] (failBuilder: FailureBuilder[E], next: Reader[E]) {
  def failMessage = failBuilder.build(next)
}

private[ll] object InterimFailureInfo {
  def apply [E] (expected: String, priority: Int, next: Reader[E]): InterimFailureInfo[E] = InterimFailureInfo(FailureBuilder(expected, { _.head.toString }, priority), next)
  def apply [E] (expected: String, next: Reader[E], read: Reader[E] => String): InterimFailureInfo[E] = InterimFailureInfo(FailureBuilder(expected, read, 0), next)

  def dummy [E] (in: Reader[E]): InterimFailureInfo[E] = InterimFailureInfo("dummy", Int.MinValue, in)
  def baseFailure [E] (in: Reader[E]): InterimFailureInfo[E] = InterimFailureInfo("base failure", Int.MinValue + 1, in)

  def best [E] (e1: InterimFailureInfo[E], e2: InterimFailureInfo[E]): InterimFailureInfo[E] = {
    if (e1.next.pos < e2.next.pos) e2
    else if (e1.next.pos == e2.next.pos && e1.failBuilder < e2.failBuilder) e2
    else e1
  }
}
