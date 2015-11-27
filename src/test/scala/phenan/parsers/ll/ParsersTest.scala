package phenan.parsers.ll

import org.scalatest._
import phenan.parsers.reader._

class ParsersTest extends FunSuite with Matchers {
  object MyParsers extends Parsers[Char] {
    def non_zero_digit = elem("digit", { n => '1' <= n && n <= '9' }) ^^ { _ - '0' }
    def digit = non_zero_digit | elem('0') ^^^ 0

    def nat = non_zero_digit ~ digit.* ^^ { case d ~ ds => ds.foldLeft(d)(_ * 10 + _) }

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
    println(parse(exprs, "6+5*2;3+a;3*b*2;6;").message)

    println(parse(exprs, "1+2+3*4/5;").message)

    println(parse(exprs, "1+5;3*4*2;").message)
  }
}
