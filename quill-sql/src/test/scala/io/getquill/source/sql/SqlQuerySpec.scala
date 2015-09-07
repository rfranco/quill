package io.getquill.source.sql

import io.getquill.Spec
import io.getquill.quote
import io.getquill.unquote
import io.getquill.norm.QueryGenerator
import io.getquill.norm.Normalize
import io.getquill.quotation.FreeVariables
import io.getquill.ast.Ident
import io.getquill.ast.Ast

class SqlQuerySpec extends Spec {

//  "transforms the ast into a flatten sql-like structure" - {
//    "random-generated query" - {
//      val gen = new QueryGenerator(1)
//      for (i <- (5 to 20)) {
//        for (j <- (0 until 30)) {
//          val query = Normalize(gen(i))
//          if (i == 6 && j == 17) {
//            s"$i levels ($j) - $query" in {
//              println(SqlQuery(query))
//              verifyQuery(SqlQuery(query))
//
//              def verifyQuery(sqlq: SqlQuery): Unit = {
//                val aliases = sqlq.from.map(_.alias).map(Ident(_)) :+ Ident("*")
//                def verifyFreeVars(ast: Ast) =
//                  (FreeVariables(ast) -- aliases) mustEqual Set()
//                sqlq.from.collect {
//                  case QuerySource(sqlq, _) => verifyQuery(sqlq)
//                }
//                sqlq.where.map(verifyFreeVars)
//                sqlq.orderBy.map(_.property).map(verifyFreeVars)
//                sqlq.limit.map(verifyFreeVars)
//                verifyFreeVars(sqlq.select)
//              }
//            }
//          }
//        }
//      }
//    }

    "non-sorted query" in {
      val q = quote {
        for {
          a <- qr1
          b <- qr2 if (a.s != null && b.i > a.i)
        } yield {
          (a, b)
        }
      }
      val sqlq = SqlQuery(q.ast)
      sqlq.from mustEqual List(TableSource("TestEntity", "a"), TableSource("TestEntity2", "b"))
      sqlq.where.toString mustEqual "Some((a.s != null) && (b.i > a.i))"
      sqlq.select.toString mustEqual "(a, b)"
      sqlq.orderBy mustEqual List()
    }
    "sorted query" - {
      "with map" in {
        val q = quote {
          qr1.sortBy(t => t.s).map(t => t.s)
        }
        val sqlq = SqlQuery(q.ast)
        sqlq.from mustEqual List(TableSource("TestEntity", "t"))
        sqlq.where mustEqual None
        sqlq.select.toString mustEqual "t.s"
        sqlq.orderBy.toString mustEqual "List(OrderByCriteria(t.s,false))"
      }
      "with filter" in {
        val q = quote {
          qr1.filter(t => t.s == "s").sortBy(t => t.s).map(t => (t.i))
        }
        val sqlq = SqlQuery(q.ast)
        sqlq.from mustEqual List(TableSource("TestEntity", "t"))
        sqlq.where.toString mustEqual """Some(t.s == "s")"""
        sqlq.select.toString mustEqual "t.i"
        sqlq.orderBy.toString mustEqual "List(OrderByCriteria(t.s,false))"
      }
      "with reverse" in {
        val q = quote {
          qr1.sortBy(t => t.s).reverse.map(t => t.s)
        }
        val sqlq = SqlQuery(q.ast)
        sqlq.from mustEqual List(TableSource("TestEntity", "t"))
        sqlq.where mustEqual None
        sqlq.select.toString mustEqual "t.s"
        sqlq.orderBy.toString mustEqual "List(OrderByCriteria(t.s,true))"
      }
      "tuple criteria" in {
        val q = quote {
          qr1.sortBy(t => (t.s, t.i)).map(t => t.s)
        }
        val sqlq = SqlQuery(q.ast)
        sqlq.from mustEqual List(TableSource("TestEntity", "t"))
        sqlq.where mustEqual None
        sqlq.select.toString mustEqual "t.s"
        sqlq.orderBy.toString mustEqual "List(OrderByCriteria(t.s,false), OrderByCriteria(t.i,false))"
      }
      "multiple sortBy" in {
        val q = quote {
          qr1.sortBy(t => (t.s, t.i)).reverse.sortBy(t => t.l).map(t => t.s)
        }
        val sqlq = SqlQuery(q.ast)
        sqlq.from mustEqual List(TableSource("TestEntity", "t"))
        sqlq.where mustEqual None
        sqlq.select.toString mustEqual "t.s"
        sqlq.orderBy.toString mustEqual "List(OrderByCriteria(t.s,true), OrderByCriteria(t.i,true), OrderByCriteria(t.l,false))"
      }
      "fails if the sortBy criteria is malformed" in {
        val q = quote {
          qr1.sortBy(t => t)(null)
        }
        val e = intercept[IllegalStateException] {
          SqlQuery(q.ast)
        }
      }
    }
  }

  "fails if the query is not normalized" in {
    val q = quote {
      qr1.map(_.s).filter(_ == "s")
    }
    val e = intercept[IllegalStateException] {
      SqlQuery(q.ast)
    }
  }
}
