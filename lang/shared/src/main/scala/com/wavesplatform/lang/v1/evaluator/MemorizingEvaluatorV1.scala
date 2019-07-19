package com.wavesplatform.lang.v1.evaluator

import cats.Monad
import cats.implicits._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext.Lenses._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.{ExecutionError, TrampolinedExecResult}

import scala.collection.mutable.ListBuffer

object MemorizingEvaluatorV1 {

  private def evalLetBlock(let: LET, inner: EXPR): MemorizedEvalM[(MemorizedLoggedEvaluationContext, EVALUATED)] =
    for {
      ctx <- get[MemorizedLoggedEvaluationContext, ExecutionError]
      blockEvaluation = evalExpr(let.value)
      lazyBlock       = MemorizingLazyVal(blockEvaluation.ter(ctx), ctx.l(let.name))
      result <- local {
        modify[MemorizedLoggedEvaluationContext, ExecutionError](mlets.modify(_)(_.updated(let.name, lazyBlock)))
          .inspectFlat(s => evalExpr(inner))
      }
    } yield result

  private def evalFuncBlock(func: FUNC, inner: EXPR): MemorizedEvalM[(MemorizedLoggedEvaluationContext, EVALUATED)] = {
    val funcHeader = FunctionHeader.User(func.name)
    val function   = UserFunction(func.name, 0, null, s"user defined function '${func.name}'", func.args.map(n => (n, null, n)): _*)(func.body)
    for {
      result <- local {
        modify[MemorizedLoggedEvaluationContext, ExecutionError](mfuncs.modify(_)(_.updated(funcHeader, function)))
          .flatMap(_ => evalExpr(inner))
      }
    } yield result
  }

  private def evalRef(key: String): MemorizedEvalM[(MemorizedLoggedEvaluationContext, EVALUATED)] =
    get[MemorizedLoggedEvaluationContext, ExecutionError] flatMap { ctx =>
      mlets.get(ctx).get(key) match {
        case Some(lzy) => mliftTER(lzy.value)
        case None      => raiseError[MemorizedLoggedEvaluationContext, ExecutionError, (MemorizedLoggedEvaluationContext, EVALUATED)](s"A definition of '$key' not found")
      }
    }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): MemorizedEvalM[(MemorizedLoggedEvaluationContext, EVALUATED)] =
    evalExpr(cond) flatMap {
      case (_, TRUE)  => evalExpr(ifTrue)
      case (_, FALSE) => evalExpr(ifFalse)
      case _          => ???
    }

  private def evalGetter(expr: EXPR, field: String): MemorizedEvalM[(MemorizedLoggedEvaluationContext, EVALUATED)] =
    evalExpr(expr).flatMap { case (ctx, exprResult) =>
      val fields = exprResult.asInstanceOf[CaseObj].fields
      fields.get(field) match {
        case Some(f) => (ctx, f).pure[MemorizedEvalM]
        case None    => raiseError(s"A definition of '$field' not found amongst ${fields.keys}")
      }
    }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): MemorizedEvalM[(MemorizedLoggedEvaluationContext, EVALUATED)] =
    for {
      ctx <- get[MemorizedLoggedEvaluationContext, ExecutionError]
      result <- mfuncs
        .get(ctx)
        .get(header)
        .map {
          case func: UserFunction =>
            args
              .traverse[MemorizedEvalM, (MemorizedLoggedEvaluationContext, EVALUATED)](evalExpr)
              .flatMap { args =>
                val letDefsWithArgs = args.zip(func.signature.args).foldLeft(ctx.ec.letDefs) {
                  case (r, (argValue, (argName, _))) => r + (argName -> MemorizingLazyVal(argValue.pure[TrampolinedExecResult], ctx.l(s"$argName")))
                }
                local {
                  set(mlets.set(ctx)(letDefsWithArgs)).flatMap(_ => evalExpr(func.ev))
                }
              }
          case func: NativeFunction =>
            args
              .traverse[MemorizedEvalM, (MemorizedLoggedEvaluationContext, EVALUATED)] { x =>
                evalExpr(x)
              }
              .map(e => (e.map(_._1).head, func.eval(e.map(_._2))))
              .flatMap(r => mliftTER[(MemorizedLoggedEvaluationContext, EVALUATED)](r._2.value.map(_.map((r._1, _)))))
        }
        .orElse(
          // no such function, try data constructor
          header match {
            case FunctionHeader.User(typeName) =>
              mtypes.get(ctx).get(typeName).collect {
                case t @ CASETYPEREF(_, fields) =>
                  args
                    .traverse[MemorizedEvalM, (MemorizedLoggedEvaluationContext, EVALUATED)](a => evalExpr(a))
                    .map(argValues => (argValues.map(_._1).head, CaseObj(t, fields.map(_._1).zip(argValues.map(_._2)).toMap)))
              }
            case _ => None
          }
        )
        .getOrElse(raiseError[MemorizedLoggedEvaluationContext, ExecutionError, (MemorizedLoggedEvaluationContext, EVALUATED)](s"function '$header' not found"))
    } yield result

  def evalExpr(t: EXPR): MemorizedEvalM[(MemorizedLoggedEvaluationContext, EVALUATED)] = t match {
    case LET_BLOCK(let, inner) => evalLetBlock(let, inner)
    case BLOCK(dec, inner) =>
      dec match {
        case l: LET  => evalLetBlock(l, inner)
        case f: FUNC => evalFuncBlock(f, inner)
      }
    case REF(str)                    => evalRef(str)
    case c: EVALUATED                => implicitly[Monad[MemorizedEvalM]].pure(
      (MemorizedLoggedEvaluationContext(_ => _ => (), MemorizedEvaluationContext(Map(), Map(), Map())), c)
    )
    case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
    case GETTER(expr, field)         => evalGetter(expr, field)
    case FUNCTION_CALL(header, args) => evalFunctionCall(header, args)
  }

  def applyWithLogging[A <: EVALUATED](c: MemorizedEvaluationContext, expr: EXPR): (Log, Either[ExecutionError, A]) = {
    val log = ListBuffer[LogItem]()
    val r   = ap(c, expr, (str: String) => (v: LetExecResult) => log.append((str, v)))
    (log.toList, r)
  }

  def applyWithLogging[A <: EVALUATED](
    c:    Either[ExecutionError, MemorizedEvaluationContext],
    expr: EXPR
  ): (Log, Either[ExecutionError, A]) = {
    val log = ListBuffer[LogItem]()
    val r = c.flatMap(ap(_, expr, (str: String) => (v: LetExecResult) => log.append((str, v))))
    (log.toList, r)
  }

  def applyWithContext(c: MemorizedEvaluationContext, expr: EXPR): (MemorizedEvaluationContext, Either[ExecutionError, (MemorizedLoggedEvaluationContext, EVALUATED)]) =
    evalExpr(expr)
      .run(MemorizedLoggedEvaluationContext(_ => _ => (), c))
      .value
      .leftMap(_.ec)

  def apply[A <: EVALUATED](c: MemorizedEvaluationContext, expr: EXPR): Either[ExecutionError, A] = ap(c, expr, _ => _ => ())

  def evalWithLogging(c: MemorizedEvaluationContext, evalC: MemorizedEvalM[EVALUATED]): (Log, Either[ExecutionError, EVALUATED]) = {
    val log = ListBuffer[LogItem]()
    val llc = (str: String) => (v: LetExecResult) => log.append((str, v))
    val lec = MemorizedLoggedEvaluationContext(llc, c)
    val res = evalC.run(lec).value._2
    (log.toList, res)
  }

  def evalWithLogging(
    ctx:   Either[ExecutionError, MemorizedEvaluationContext],
    evalC: MemorizedEvalM[EVALUATED]
  ): (Log, Either[ExecutionError, EVALUATED]) = {
    val log = ListBuffer[LogItem]()
    val llc = (str: String) => (v: LetExecResult) => log.append((str, v))
    val res = ctx.map(MemorizedLoggedEvaluationContext(llc, _))
      .flatMap(evalC.run(_).value._2)
    (log.toList, res)
  }

  private def ap[A <: EVALUATED](c: MemorizedEvaluationContext, expr: EXPR, llc: LetLogCallback): Either[ExecutionError, A] = {
    val lec = MemorizedLoggedEvaluationContext(llc, c)
    evalExpr(expr)
      .map(_.asInstanceOf[A])
      .run(lec)
      .value
      ._2
  }

}
