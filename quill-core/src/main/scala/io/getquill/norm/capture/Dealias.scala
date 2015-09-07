package io.getquill.norm.capture

import io.getquill.ast.Ident
import io.getquill.ast._
import io.getquill.ast.StatefulTransformer
import io.getquill.norm.BetaReduction

private[capture] case class Dealias(state: Option[Ident]) extends StatefulTransformer[Option[Ident]] {

  override def apply(q: Query): (Query, StatefulTransformer[Option[Ident]]) =
    q match {
      case Map(a, b, c) =>
        apply(a) match {
          case (an, t @ Dealias(Some(alias))) =>
            (Map(an, alias, BetaReduction(c, b -> alias)), t)
          case other =>
            (q, Dealias(Some(b)))
        }
      case FlatMap(a, b, c) =>
        apply(a) match {
          case (an, t @ Dealias(Some(alias))) =>
            (FlatMap(an, alias, BetaReduction(c, b -> alias)), t)
          case other =>
            (q, Dealias(Some(b)))
        }
      case Filter(a, b, c) =>
        apply(a) match {
          case (an, t @ Dealias(Some(alias))) =>
            (Filter(an, alias, BetaReduction(c, b -> alias)), t)
          case other =>
            (q, Dealias(Some(b)))
        }
      case SortBy(a, b, c) =>
        apply(a) match {
          case (an, t @ Dealias(Some(alias))) =>
            (SortBy(an, alias, BetaReduction(c, b -> alias)), t)
          case other =>
            (q, Dealias(Some(b)))
        }
      case Take(a, n) =>
        val (an, ant) = apply(a)
        (Take(an, n), ant)
      case Reverse(a) =>
        val (an, ant) = apply(a)
        (Reverse(an), ant)
      case q: Entity =>
        (q, Dealias(None))
    }
}

object Dealias {
  def apply(query: Query) =
    new Dealias(None)(query) match {
      case (q, _) => q
    }
}
