より良いエラーメッセージを出すパーサコンビネータ
====

Scala の標準ライブラリのパーサコンビネータはエラーメッセージが残念なので、
もう少しマシなエラーメッセージが出るパーサコンビネータを実装しました。

例
----

こんな感じのエラーメッセージが出せます。

    [1, 3] expected natural number, but found a
    > 1+a+3
    >   ^
 
`panic` という機能を使うことで、一度に複数のエラーを検出することもできます。
この機能は構文解析エラー箇所からある程度読み飛ばしを行うことでエラーからの回復を図るものです。
 
    [1, 3] expected natural number, but found a
    > 1+a+3
    >   ^
    [2, 5] expected natural number, but found b
    > 1*2*b
    >     ^
 
使い方
----

基本的には Scala の標準ライブラリの Parsers と似ていますが、細かいところがやや異なります。
また、`implicit` な関数は実装してないのでやや冗長です。

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

最も特徴的なのは `panic` で、引数に指定したパーサを使ってエラー箇所からの回復を行います。
`panic` によるエラー回復はエラー情報を消すものではなく、他のエラーを見つけるために一時的に強引に正常状態に戻すものです。


PackratParsers は Scala の標準ライブラリのものと同じく左再帰をサポートしています。


原理
----

Scala の標準ライブラリのパーサコンビネータのエラーメッセージが残念だったのは、構文解析が成功した時にそれまでの失敗に関する情報を忘れてしまうためでした。
例えば、以下のようなパーサを考えます。

    def block = elem('{') ~> statement.* <~ elem('}')
    
この場合 `statement.*` は必ず成功するため、エラーメッセージは必ず `'}' is not found` になってしまいます。


そのため、このライブラリでは成功した場合でも「最良の失敗」を常に覚えておくことで、より良いエラーメッセージを出せるようにしました。
上の例であれば、`statement.*` は必ず成功しますが、最後に失敗した `statement` の情報を覚えているので、
`'}' is not found` と比較してより良い方のエラーメッセージを出力できます。


作者
----
@phenan
