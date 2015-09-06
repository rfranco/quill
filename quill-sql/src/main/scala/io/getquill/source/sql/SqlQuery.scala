package io.getquill.source.sql

import io.getquill.ast.Ast
import io.getquill.ast.Entity
import io.getquill.ast.Filter
import io.getquill.ast.FlatMap
import io.getquill.ast.Ident
import io.getquill.ast.Map
import io.getquill.ast.Query
import io.getquill.ast.SortBy
import io.getquill.util.Messages.fail
import io.getquill.ast.Property
import io.getquill.ast.Tuple
import io.getquill.ast.Reverse
import io.getquill.ast.Take
import io.getquill.norm.select.ReplaceSelect

sealed trait Source
case class TableSource(table: String, alias: String) extends Source
case class QuerySource(query: SqlQuery, alias: String) extends Source

case class OrderByCriteria(property: Property,
                           desc: Boolean)

case class SqlQuery(from: List[Source],
                    where: Option[Ast],
                    orderBy: List[OrderByCriteria],
                    select: Ast,
                    limit: Option[Ast])

object SqlQuery {

  def apply(query: Ast): SqlQuery =
    query match {

      // entity.*

      case Map(Entity(name), Ident(alias), p) =>
        SqlQuery(
          from = List(TableSource(name, alias)),
          where = None,
          orderBy = List(),
          select = p,
          limit = None)

      case FlatMap(Entity(name), Ident(alias), r: Query) =>
        val nested = apply(r)
        nested.copy(from = TableSource(name, alias) :: nested.from)

      case Filter(Entity(name), Ident(alias), p) =>
        SqlQuery(
          from = TableSource(name, alias) :: Nil,
          where = Option(p),
          orderBy = List(),
          select = Ident(alias),
          limit = None)

      case Reverse(SortBy(Entity(name), Ident(alias), p)) =>
        SqlQuery(
          from = List(TableSource(name, alias)),
          where = None,
          orderBy = orderByCriterias(p, reverse = true),
          select = Ident(alias),
          limit = None)

      case SortBy(Entity(name), Ident(alias), p) =>
        SqlQuery(
          from = List(TableSource(name, alias)),
          where = None,
          orderBy = orderByCriterias(p, reverse = false),
          select = Ident(alias),
          limit = None)

      case Take(Entity(name), n) =>
        SqlQuery(
          from = List(TableSource(name, "x")),
          where = None,
          orderBy = List(),
          select = Ident("*"),
          limit = Some(n))

      // entity.*.take

      // take.*

      case Map(t: Take, Ident(alias), p) =>
        apply(t).copy(select = p)

      case FlatMap(t: Take, Ident(alias), r: Query) =>
        val nested = apply(r)
        nested.copy(from = querySource(t, alias) :: nested.from)

      case Filter(t: Take, Ident(alias), p) =>
        SqlQuery(
          from = querySource(t, alias) :: Nil,
          where = Option(p),
          orderBy = List(),
          select = Ident(alias),
          limit = None)

      case Reverse(SortBy(t: Take, Ident(alias), p)) =>
        SqlQuery(
          from = List(querySource(t, alias)),
          where = None,
          orderBy = orderByCriterias(p, reverse = true),
          select = Ident(alias),
          limit = None)

      case SortBy(t: Take, Ident(alias), p) =>
        SqlQuery(
          from = List(querySource(t, alias)),
          where = None,
          orderBy = orderByCriterias(p, reverse = false),
          select = Ident(alias),
          limit = None)

      // nested

      case Map(q: Query, x, p) =>
        apply(q).copy(select = p)

      case Reverse(SortBy(q: Query, Ident(alias), p)) =>
        val base = apply(q)
        SqlQuery(
          from = base.from,
          where = base.where,
          orderBy = base.orderBy ++ orderByCriterias(p, reverse = true),
          select = Ident(alias),
          limit = None)

      case SortBy(q: Query, Ident(alias), p) =>
        val base = apply(q)
        SqlQuery(
          from = base.from,
          where = base.where,
          orderBy = base.orderBy ++ orderByCriterias(p, reverse = false),
          select = Ident(alias),
          limit = None)

      case Take(q, n) =>
        apply(q).copy(limit = Some(n))

      case other =>
        fail(s"Query is not propertly normalized, please submit a bug report. $query")
    }

  private def querySource(query: Query, alias: String) =
    QuerySource(SqlQuery(query).copy(select = Ident("*")), alias)

  private def orderByCriterias(ast: Ast, reverse: Boolean): List[OrderByCriteria] =
    ast match {
      case a: Property       => List(OrderByCriteria(a, reverse))
      case Tuple(properties) => properties.map(orderByCriterias(_, reverse)).flatten
      case other             => fail(s"Invalid order by criteria $ast")
    }
}
