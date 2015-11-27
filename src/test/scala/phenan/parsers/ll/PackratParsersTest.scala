package phenan.parsers.ll

import org.scalatest._
import phenan.parsers.reader._

class PackratParsersTest extends FunSuite with Matchers {
  object MyParsers extends PackratParsers[Char] {
    lazy val non_zero_digit = elem("digit", { n => '1' <= n && n <= '9' }) ^^ { _ - '0' }
    lazy val digit = non_zero_digit | elem('0') ^^^ 0

    lazy val nat = non_zero_digit ~ digit.* ^^ { case d ~ ds => ds.foldLeft(d)(_ * 10 + _) } as "natural number"

    lazy val mul: PackratParser[Char, Int] = memo(mul_op | div_op | nat)
    lazy val mul_op = ( mul <~ elem('*') ) ~ nat ^^ { case a ~ b => a * b }
    lazy val div_op = ( mul <~ elem('/') ) ~ nat ^^ { case a ~ b => a / b }

    lazy val add: PackratParser[Char, Int] = memo(add_op | sub_op | mul)
    lazy val add_op = ( add <~ elem('+') ) ~ mul ^^ { case a ~ b => a + b }
    lazy val sub_op = ( add <~ elem('-') ) ~ mul ^^ { case a ~ b => a - b }

    lazy val expr = ( add <~ elem(';') ).panic(untilSemicolon)

    lazy val untilSemicolon = elem("", _ != ';').* ~> elem(';')

    lazy val exprs = expr.+
  }

  def parse [T] (parser: Parser[Char, T], src: String): ParseResult[T] = parser.parseAll(CharSequenceReader(src))

  import MyParsers._

  test ("digit") {
    println(parse(exprs, "6+5*2;3+a;3*b*2;6;").message)

    println(parse(exprs, "1+2+3*4/5;").message)

    println(parse(exprs, "3*4*2;2+3*4;").message)
  }
}
