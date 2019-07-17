package com.wavesplatform.lang

import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.{Matchers, PropSpec}

class ReplTest extends PropSpec with ScriptGen with Matchers with NoShrink {
  property("variable memorization") {
    val repl = Repl()
    repl.execute("let a = 1")     shouldBe Right("Unit")
    repl.execute("let b = 2")     shouldBe Right("Unit")
    repl.execute("let c = a + b") shouldBe Right("Unit")
    repl.execute("c")             shouldBe Right("3")
    repl.execute("a + b")         shouldBe Right("3")
  }

  property("context funcs") {
    val repl = Repl()
    repl.execute(""" let s = "aaa|bbb|ccc" """)
    repl.execute(""" s.split("|") """) shouldBe Right("""["aaa", "bbb", "ccc"]""")

    repl.execute(""" let a = blake2b256(base58'') != base58'' """)
    repl.execute(""" a || false """) shouldBe Right("true")
  }

  property("user funcs") {
    val repl = Repl()
    repl.execute(""" func inc(a: Int) = a + 1 """) shouldBe Right("Unit")
    repl.execute(""" inc(5) """) shouldBe Right("6")
  }

  property("syntax errors") {
    val repl = Repl()
    repl.execute(""" let a = {{1} """) shouldBe Left("Compilation failed: expected a value's expression in 9-9")
    repl.execute(""" 1 ++ 2 """)       shouldBe Left("Compilation failed: expected a second operator in 4-4")
  }

  property("logic errors") {
    val repl = Repl()
    repl.execute(""" let a = base64'12345' """) shouldBe Left("Compilation failed: can't parse Base64 string in 17-21")
    repl.execute(""" let b = "abc" + 1 """)     shouldBe Left("Compilation failed: Can't find a function overload '+'(String, Int) in 9-18")
  }

  property("exceptions") {
    val repl = Repl()
    val msg = "error message"
    repl.execute(s""" throw("$msg") """) shouldBe Left(msg)
  }
}
