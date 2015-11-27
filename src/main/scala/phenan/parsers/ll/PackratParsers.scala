package phenan.parsers.ll

import phenan.parsers.reader._

import scala.collection.mutable

/**
  * Created by ichikawa on 2015/11/21.
  */
trait PackratParsers[E] extends Parsers[E] {
  def memo[T] (parser: => Parser[E, T]): PackratParser[E, T] = new PackratParserImpl[T](parser)

  class PackratParserImpl [+T] (parser: => Parser[E, T]) extends PackratParser[E, T] {
    lazy val underlying: Parser[E, T] = ParserImpl { in => parser(in) }

    def map [U] (f: T => U): Parser[E, U] = ParserImpl { in => apply(in).map(f) }
    def seq [U] (f: => Parser[E, U]): Parser[E, T ~ U] = ParserImpl { in => apply(in).seq(f.apply) }
    def orElse [U >: T] (f: => Parser[E, U]): Parser[E, U] = ParserImpl { in => apply(in).orElse(f(in)) }
    def select [U >: T] (f: => Parser[E, U]): Parser[E, U] = ParserImpl { in => apply(in).longest(f(in)) }
    def optional: Parser[E, Option[T]] = map { Some(_) } orElse success(None)
    def panic [U] (f: => Parser[E, U]): Parser[E, T] = ParserImpl { in => apply(in).panic(f.apply) }
    def catches (f: FailureBuilder[E] => FailureBuilder[E]): Parser[E, T] = ParserImpl { in => apply(in).catches(f) }
  }
}

trait PackratParser[E, +T] extends Parser[E, T] {
  def underlying: Parser[E, T]

  private[ll] def apply (in: Reader[E]): InterimResult[E, T] = recall(in) match {
    case Some(Right(result)) => result
    case Some(Left(lr))      => lr.setup(this, in)
    case None                => applyRule(in)
  }

  private def recall (in: Reader[E]): Option[Either[LR[E, T], InterimResult[E, T]]] = (PackratState.recursionHead(in), PackratState.lookupMemo(this, in)) match {
    case (Some(head), None) if ! head.involved(this)   => Some(Right(InterimFailure(InterimFailureInfo.dummy(in), Nil)))
    case (Some(head), _) if head.isRecursiveCall(this) => Some(head.reevaluate(this, in))
    case (_, cached)                                   => cached
  }

  private def applyRule (in: Reader[E]): InterimResult[E, T] = {
    val lr = LR(this, InterimFailure(InterimFailureInfo.baseFailure(in), Nil), None)

    PackratState.memoize(this, in, Left(lr))
    val ans = PackratState.applyWithLRFrame(lr, this, in)
    PackratState.memoize(this, in, Right(ans))

    lr.getHead match {
      case Some(head) => lrAnswer(in, head, ans)
      case None       => ans
    }
  }

  private def lrAnswer [R >: T] (in: Reader[E], head: Head[E, _], ans: InterimResult[E, R]): InterimResult[E, R] = {
    if (head.parser != this) ans
    else growLR(in, head, ans)
  }

  private def growLR [R >: T] (in: Reader[E], head: Head[E, _], seed: InterimResult[E, R]): InterimResult[E, R] = {
    PackratState.setupRecursion(in, head)

    val s = underlying(in)

    if (s.isBetterThan(seed)) {
      val result = s.withFailInfo(seed.bestFailure)
      PackratState.memoize(this, in, Right(result))
      growLR(in, head, result)
    }
    else {
      PackratState.endRecursion(in)
      val result = seed.withFailInfo(s.bestFailure)
      PackratState.memoize(this, in, Right(result))
      result
    }
  }
}

private[ll] class PackratState[E] {
  private val memos: mutable.Map[PackratParser[E, _], Either[LR[E, _], InterimResult[E, _]]] = mutable.Map.empty
  private val lrStack: mutable.Stack[LR[E, _]] = new mutable.Stack
  private var recHead: Option[Head[E, _]] = None
}

private[ll] object PackratState {
  def lookupMemo [E, T] (parser: PackratParser[E, T], reader: Reader[E]): Option[Either[LR[E, T], InterimResult[E, T]]] = {
    apply(reader).memos.get(parser).asInstanceOf[Option[Either[LR[E, T], InterimResult[E, T]]]]
  }

  def memoize [E, T] (parser: PackratParser[E, T], reader: Reader[E], value: Either[LR[E, T], InterimResult[E, T]]): Unit = {
    apply(reader).memos.put(parser, value)
  }

  def applyWithLRFrame [E, T] (lr: LR[E, T], parser: PackratParser[E, T], in: Reader[E]): InterimResult[E, T] = {
    val stack = apply(in).lrStack
    stack.push(lr)
    val ans = parser.underlying(in)
    stack.pop()
    ans
  }

  def recursionMembers [E] (p: PackratParser[E, _], reader: Reader[E]): List[LR[E, _]] = apply(reader).lrStack.takeWhile(_.parser != p).toList

  def recursionHead [E] (reader: Reader[E]): Option[Head[E, _]] = apply(reader).recHead

  def setupRecursion [E] (reader: Reader[E], head: Head[E, _]): Unit = {
    apply(reader).recHead = Some(head)
    head.setupRecursion()
  }

  def endRecursion [E] (reader: Reader[E]): Unit = {
    apply(reader).recHead = None
  }

  def apply [E] (reader: Reader[E]): PackratState[E] = weakMap.getOrElseUpdate(reader, new PackratState[E]).asInstanceOf[PackratState[E]]

  def checkEq [E, T] (parser: PackratParser[E, T], in: Reader[E], seed: InterimResult[E, T]) = {
    val memo = lookupMemo(parser, in)
    if (! memo.contains(Right(seed))) println("break memo")
  }

  private val weakMap: mutable.WeakHashMap[Reader[_], PackratState[_]] = mutable.WeakHashMap.empty
}

private[ll] case class LR[E, +T] (parser: PackratParser[E, T], seed: InterimResult[E, T], private var head: Option[Head[E, _]]) {
  def getHead = head

  def setup [R >: T] (p: PackratParser[E, R], in: Reader[E]): InterimResult[E, R] = {
    if (head.isEmpty) head = Some(Head(p))
    for (s <- PackratState.recursionMembers(p, in)) {
      s.head = head
      head.foreach(_.involve(s.parser))
    }
    seed
  }
}

private[ll] case class Head[E, T] (parser: PackratParser[E, T]) {
  private val involvedSet: mutable.ListBuffer[PackratParser[E, _]] = mutable.ListBuffer.empty
  private val evalSet: mutable.ListBuffer[PackratParser[E, _]] = mutable.ListBuffer.empty

  def involve (p: PackratParser[E, _]): Unit = { involvedSet += p }
  def involved (p: PackratParser[E, _]): Boolean = involvedSet.contains(p) || parser == p

  def isRecursiveCall (p: PackratParser[E, _]): Boolean = evalSet.contains(p)

  def setupRecursion(): Unit = {
    evalSet.clear()
    evalSet ++= involvedSet
  }

  def reevaluate [R] (p: PackratParser[E, R], in: Reader[E]) = {
    evalSet -= p
    PackratState.lookupMemo(p, in) match {
      case Some(Right(r)) =>
        val result = Right(p.underlying(in).withFailInfo(r.bestFailure))
        PackratState.memoize(p, in, result)
        result
      case _              =>
        val result = Right(p.underlying(in))
        PackratState.memoize(p, in, result)
        result
    }
  }
}
