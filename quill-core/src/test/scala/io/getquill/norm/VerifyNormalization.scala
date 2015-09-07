package io.getquill.norm

import io.getquill.ast.Entity
import io.getquill.ast.Filter
import io.getquill.ast.FlatMap
import io.getquill.ast.Map
import io.getquill.ast.Query
import io.getquill.ast.SortBy
import io.getquill.ast.StatelessTransformer
import io.getquill.util.Messages.fail
import io.getquill.ast.Reverse
import io.getquill.ast.Take

object VerifyNormalization extends StatelessTransformer {

  override def apply(q: Query) = verifyFinalFlatMapBody(finalFlatMapBody(q))

  private def finalFlatMapBody(q: Query): Query =
    q match {
      case FlatMap(Take(a, n), b, c: FlatMap) =>
        apply(a)
        finalFlatMapBody(c)
      case FlatMap(Take(a, n), b, c: Query) =>
        apply(a)
        verifyFinalFlatMapBody(c)
      case FlatMap(a: Entity, b, c: FlatMap) => finalFlatMapBody(c)
      case FlatMap(a: Entity, b, c: Query)   => verifyFinalFlatMapBody(c)
      case other                             => verifyFinalFlatMapBody(q)
    }

  private def verifyFinalFlatMapBody(q: Query): Query =
    q match {
      case Map(a: Query, b, c) => verifySortByClauses(a)
      case other               => verifySortByClauses(q)
    }

  private def verifySortByClauses(q: Query): Query =
    q match {
      case Reverse(SortBy(a: Query, b, c)) => verifySortByClauses(a)
      case SortBy(a: Query, b, c)          => verifySortByClauses(a)
      case other                           => verifyFilterClause(q)
    }

  private def verifyFilterClause(q: Query): Query =
    q match {
      case q: Filter         => q
      case q: Entity         => q
      case Take(a: Query, n) => apply(a)
      case other             => fail(s"Expected 'Filter' or 'Entity', but got $q")
    }
}
