package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler, Terms, Types}
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, MemorizingEvaluatorV1}
import com.wavesplatform.lang.v1.evaluator.ctx.{MemorizedEvaluationContext, MemorizedLoggedEvaluationContext, MemorizingLazyVal}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Expressions.{BLOCK, EXPR, INVALID, REF}
import com.wavesplatform.lang.v1.parser.Parser
import monix.execution.atomic.Atomic
import cats.implicits._

case class Repl(ver: StdLibVersion = V3) {
  private val Global: BaseGlobal = com.wavesplatform.lang.Global
  private val initialCtx         = CryptoContext.build(Global, ver) |+| PureContext.build(Global, ver)
  private val ctx                = Atomic((
    initialCtx.compilerContext,
    MemorizedEvaluationContext(
      initialCtx.evaluationContext.typeDefs,
      initialCtx.evaluationContext.letDefs.mapValues(v => MemorizingLazyVal.applyM(EitherT(v.value))),
      initialCtx.evaluationContext.functions
    )
  ))

  def execute(expr: String): Either[String, String] =
    ctx.transformAndExtract { case (compileCtx, evalCtx) =>
      evalExpr(expr, compileCtx, evalCtx).fold(
        e => (Left(e), (compileCtx, evalCtx)),
        { case (r, newCompileCtx, newEvalCtx) => (Right(r), (newCompileCtx, newEvalCtx)) }
      )
    }

  private def evalExpr(
      expr:       String,
      compileCtx: CompilerContext,
      evalCtx:    MemorizedEvaluationContext
  ): Either[String, (String, CompilerContext, MemorizedEvaluationContext)] =
    for {
      parsed <- Parser.parseExpr(expr).fold(
        { case (_, _, err) => Left(err.traced.toString) },
        { case (result, _) => Right(result)             }
      )
      (newCompileCtx, (compiled, _)) <- compile(compileCtx, parsed)
      (newEvalCtx, eval)             <- evaluate(evalCtx, compiled)
    } yield (eval.prettyString(0), newCompileCtx, newEvalCtx.ec)

  private def compile(
    compileCtx: CompilerContext,
    parsed:     EXPR
  ): Either[String, (CompilerContext, (Terms.EXPR, Types.FINAL))] = {
    val exprOrDecl = resolveDeclaration(parsed)
    ExpressionCompiler.applyWithContext(compileCtx, exprOrDecl).bitraverse(Right(_), identity)
  }

  private def resolveDeclaration(parsed: EXPR): EXPR =
    parsed match {
      case BLOCK(bpos, decl, b: BLOCK)        => BLOCK(bpos, decl, resolveDeclaration(b))
      case BLOCK(bpos, decl, INVALID(pos, _)) => BLOCK(bpos, decl, REF(pos, VALID(pos, "unit")))
      case a                                  => a
    }

  private def evaluate(
      evalCtx:  MemorizedEvaluationContext,
      compiled: Terms.EXPR
  ): Either[ExecutionError, (MemorizedLoggedEvaluationContext, Terms.EVALUATED)] =
    MemorizingEvaluatorV1.applyWithContext(evalCtx, compiled)._2
}
