package com.wavesplatform.lang.v1.evaluator.ctx

import cats.Eval
import cats.implicits._
import com.wavesplatform.lang.{ExecutionError, TrampolinedExecResult}
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.LogCallback

sealed trait MemorizingLazyVal {
  val value: Eval[Either[ExecutionError, (MemorizedLoggedEvaluationContext, EVALUATED)]]
}

object MemorizingLazyVal {
  private case class MemorizingLazyValImpl(
      v: Eval[Either[ExecutionError, (MemorizedLoggedEvaluationContext, EVALUATED)]],
      lc: LogCallback
  ) extends MemorizingLazyVal {
    override val value: Eval[Either[ExecutionError, (MemorizedLoggedEvaluationContext, EVALUATED)]] =
      v.memoize
  }

  def apply(v: TrampolinedExecResult[(MemorizedLoggedEvaluationContext, EVALUATED)], lc: LogCallback = _ => ()): MemorizingLazyVal =
    MemorizingLazyValImpl(v.value, lc)

  def applyM(v: TrampolinedExecResult[EVALUATED], lc: LogCallback = _ => ()): MemorizingLazyVal =
    MemorizingLazyValImpl(v.value.map(_.map(v => (MemorizedLoggedEvaluationContext(_ => _ => (), MemorizedEvaluationContext(Map(), Map(), Map())), v))), lc)
}
