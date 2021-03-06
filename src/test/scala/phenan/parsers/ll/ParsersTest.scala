package phenan.parsers.ll

import org.scalatest._
import phenan.parsers.reader._

class ParsersTest extends FunSuite with Matchers {
  object MyParsers extends Parsers[Char] {
    def non_zero_digit = elem("non zero digit", { n => '1' <= n && n <= '9' }) ^^ { _ - '0' }
    def digit = non_zero_digit | elem('0') ^^^ 0 as "digit"

    def nat = non_zero_digit ~ digit.* ^^ { case d ~ ds => ds.foldLeft(d)(_ * 10 + _) } as "natural number"

    def mul = nat.chainl1(mul_op | div_op)
    def mul_op = elem('*') ^^^ { (a: Int, b: Int) => a * b }
    def div_op = elem('/') ^^^ { (a: Int, b: Int) => a / b }

    def add = mul.chainl1(add_op | sub_op)
    def add_op = elem('+') ^^^ { (a: Int, b: Int) => a + b }
    def sub_op = elem('-') ^^^ { (a: Int, b: Int) => a - b }

    def expr = ( add <~ elem(';') ).panic(untilSemicolon)

    def untilSemicolon = elem("", _ != ';').* ~> elem(';')

    def exprs = expr.+
  }

  def parse [T] (parser: Parser[Char, T], src: String): ParseResult[T] = parser.parseAll(CharSequenceReader(src))

  import MyParsers._

  test ("digit") {
    parse(digit, "4") shouldBe ParseSuccess(4)
    parse(digit, "r") should matchPattern {
      case ParseFailure(List(FailureMessage("digit", "r", p))) if p.line == 1 && p.column == 1 =>
    }
  }

  test ("natural number") {
    parse(nat, "308") shouldBe ParseSuccess(308)
    parse(nat, "097") should matchPattern {
      case ParseFailure(List(FailureMessage("natural number", "0", p))) if p.line == 1 && p.column == 1 =>
    }
  }

  test ("expression") {
    parse(expr, "12/4+3*2;") shouldBe ParseSuccess(9)
    parse(expr, "12/4+3*2") should matchPattern {
      case ParseFailure(List(FailureMessage(";", "end of input", p))) if p.line == 1 && p.column == 9 =>
    }
  }

  test ("expressions") {
    parse(exprs, "1+5;3*4*2;") shouldBe ParseSuccess(List(6, 24))
    parse(exprs, "6+5*2;3+a;3*b*2;6") should matchPattern {
      case ParseFailure(List(FailureMessage("natural number", "a", _), FailureMessage("natural number", "b", _), FailureMessage(";", "end of input", _))) =>
    }
  }
}
