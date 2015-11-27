package phenan.parsers.ll

/**
  * Created by ichikawa on 2015/11/18.
  */
import phenan.parsers.reader._

/** 基本的な再帰下降型構文解析器を提供する。
  *
  * @tparam E トークン列の要素の型
  */
trait Parsers [E] {
  def success[T] (v: T): Parser[E, T] = ParserImpl { in => InterimSuccess(v, in, InterimFailureInfo.baseFailure(in)) }
  def failure (expected: String, find: Reader[E] => String): Parser[E, Nothing] = ParserImpl { in => InterimFailure(InterimFailureInfo(expected, in, find), Nil) }

  def acceptIf (f: E => Boolean)(expected: String, find: E => String): Parser[E, E] = ParserImpl { in =>
    if (! in.atEnd && f(in.head)) InterimSuccess(in.head, in.tail, InterimFailureInfo.baseFailure(in))
    else InterimFailure(InterimFailureInfo(expected, in, { n => find(n.head).toString } ), Nil)
  }

  def elem (e: E): Parser[E, E] = acceptIf(_ == e)(e.toString, { _.toString })
  def elem (kind: String, f: E => Boolean) = acceptIf(f)(kind, { _.toString } )

  private[ll] class ParserImpl [+T] (parse: Reader[E] => InterimResult[E, T]) extends Parser[E, T] {
    def apply (in: Reader[E]): InterimResult[E, T] = parse(in)
    def map [U] (f: T => U): Parser[E, U] = ParserImpl { in => parse(in).map(f) }
    def seq [U] (f: => Parser[E, U]): Parser[E, T ~ U] = ParserImpl { in => parse(in).seq(f.apply) }
    def orElse [U >: T] (f: => Parser[E, U]): Parser[E, U] = ParserImpl { in => parse(in).orElse(f(in)) }
    def select [U >: T] (f: => Parser[E, U]): Parser[E, U] = ParserImpl { in => parse(in).longest(f(in)) }
    def optional: Parser[E, Option[T]] = map { Some(_) } orElse success(None)
    def panic [U] (f: => Parser[E, U]): Parser[E, T] = ParserImpl { in => parse(in).panic(f.apply) }
    def catches (f: FailureBuilder[E] => FailureBuilder[E]): Parser[E, T] = ParserImpl { in => parse(in).catches(f) }
  }
  private[ll] object ParserImpl {
    def apply [T] (parse: Reader[E] => InterimResult[E, T]): Parser[E, T] = new ParserImpl[T](parse)
  }
}

/** 再帰下降型構文解析器を表現する。
  *
  * @tparam E トークン列の要素の型
  * @tparam T 構文解析結果の型
  */
trait Parser [E, +T] {
  private[ll] def apply (in: Reader[E]): InterimResult[E, T]

  def map [U] (f: T => U): Parser[E, U]
  def seq [U] (f: => Parser[E, U]): Parser[E, T ~ U]
  def orElse [U >: T] (f: => Parser[E, U]): Parser[E, U]
  def select [U >: T] (f: => Parser[E, U]): Parser[E, U]
  def optional: Parser[E, Option[T]]
  def panic [U] (f: => Parser[E, U]): Parser[E, T]
  def catches (f: FailureBuilder[E] => FailureBuilder[E]): Parser[E, T]

  def rep : Parser[E, List[T]] = rep1.optional.map { _.getOrElse(Nil) }
  def rep1: Parser[E, List[T]] = seq(rep).map { case r ~ rs => r :: rs }
  def repsep  (sep: => Parser[E, Nothing]): Parser[E, List[T]] = rep1sep(sep).optional.map { _.getOrElse(Nil) }
  def rep1sep (sep: => Parser[E, Nothing]): Parser[E, List[T]] = seq((sep ~> repsep(sep)).optional).map { case r ~ rs => rs.fold(List(r))(r :: _) }
  def chainl1 [U >: T] (op: => Parser[E, (U, U) => U]): Parser[E, U] = seq(op.seq(this).rep).map { case x ~ xs => xs.foldLeft[U](x) { case (a, f ~ b) => f(a, b) } }

  def as (name: String): Parser[E, T] = catches(fb => FailureBuilder(name, fb.find, fb.priority))

  def parseAll (reader: Reader[E]): ParseResult[T] = apply(reader) match {
    case InterimSuccess(r, n, bf) if n.pos < bf.next.pos                      => ParseFailure(List(bf.failMessage))
    case InterimSuccess(r, _, _)                                              => ParseSuccess(r)
    case PseudoSuccess(es, n, bf) if ! es.contains(bf) && n.pos < bf.next.pos => ParseFailure((bf :: es).map(_.failMessage))
    case PseudoSuccess(es, _, _)                                              => ParseFailure(es.map(_.failMessage))
    case InterimFailure(fail, errors)                                         => ParseFailure((fail :: errors).map(_.failMessage))
  }

  def ^^ [U] (f: T => U): Parser[E, U] = map(f)
  def ^^^ [U] (f: => U): Parser[E, U] = map(_ => f)
  def ~ [U] (f: => Parser[E, U]): Parser[E, T ~ U] = seq(f)
  def ~> [U] (that: => Parser[E, U]): Parser[E, U] = seq(that).map { _.second }
  def <~ [U] (that: => Parser[E, U]): Parser[E, T] = seq(that).map { _.first }
  def | [U >: T] (f: => Parser[E, U]): Parser[E, U] = orElse(f)
  def ||| [U >: T] (f: => Parser[E, U]): Parser[E, U] = select(f)
  def ? : Parser[E, Option[T]] = optional

  def * : Parser[E, List[T]] = rep
  def + : Parser[E, List[T]] = rep1
  def * (sep: => Parser[E, Nothing]): Parser[E, List[T]] = repsep(sep)
  def + (sep: => Parser[E, Nothing]): Parser[E, List[T]] = rep1sep(sep)
}
