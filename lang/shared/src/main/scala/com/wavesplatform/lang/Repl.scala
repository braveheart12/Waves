package com.wavesplatform.lang

import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import monix.execution.atomic.Atomic
import cats.implicits._
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Expressions.{BLOCK, EXPR, INVALID, REF}
import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Parser

case class Repl(ver: StdLibVersion = V3) {
  private val Global: BaseGlobal = com.wavesplatform.lang.Global
  private val rideCtx = CryptoContext.build(Global, ver) |+| PureContext.build(Global, ver)
  private val evalCtx = Atomic(rideCtx.evaluationContext)
  private val compileCtx = Atomic(rideCtx.compilerContext)

  def execute(expr: String): Either[String, String] = for {
    parsed <- Parser.parseExpr(expr)
      .fold(
        { case (_, _, err) => Left(err.traced.toString) },
        { case (result, _) => Right(result) }
      )
    (newCompileCtx, (compiled, _)) <- ExpressionCompiler.applyWithContext(compileCtx.get, resolveDeclaration(parsed)).bitraverse(Right(_), identity)
    (newEvalCtx, r)                <- EvaluatorV1.applyWithContext(evalCtx.get, compiled).bitraverse(Right(_), identity)
  } yield (evalCtx.set(newEvalCtx), compileCtx.set(newCompileCtx), r.prettyString(0))._3

  private def resolveDeclaration(parsed: EXPR) = parsed match {
    case BLOCK(bpos, decl, INVALID(pos, _)) => BLOCK(bpos, decl, REF(pos, VALID(pos, "unit")))
    case a => a
  }
}
