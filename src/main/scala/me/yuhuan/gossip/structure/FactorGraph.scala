package me.yuhuan.gossip.structure

import me.yuhuan.gossip.coll._
import me.yuhuan.gossip._

import scala.reflect.ClassTag



/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class FactorGraph[V: ClassTag](val vars: Set[Var], val factors: Set[Factor[V]]) extends AdjacencyListUndirectedBipartiteGraph[Factor[V], Var, Unit] {

  def keys1: Set[Factor[V]] = factors
  def keys2: Set[Var] = vars

  def adjMap1: Map[Factor[V], Set[Var]] = factors.map(f => f -> f.vars.toSet).toMap
  def adjMap2: Map[Var, Set[Factor[V]]] = invert(adjMap1)

  def dataAt(k1: Factor[V], k2: Var): Option[Unit] = Some(())

}


object FactorGraph {

  def apply[V](φs: Factor[V]*)(implicit ct: ClassTag[V]): FactorGraph[V] = {
    new FactorGraph[V](φs.flatMap(_.vars).toSet, φs.toSet)
  }

}
