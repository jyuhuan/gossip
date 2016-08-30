package me.yuhuan.gossip.bp

import me.yuhuan.gossip.coll.AdjacencyListBiDirectedBipartiteGraph
import me.yuhuan.gossip.structure._

import scala.reflect.ClassTag

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class BeliefPropagationGraph[V: ClassTag](fg: FactorGraph[V]) extends AdjacencyListBiDirectedBipartiteGraph[Factor[V], Var, F2VMessage[V], V2FMessage[V]] {

  val v2fMsgs: Map[(Var, Factor[V]), V2FMessage[V]] = fg.arcs21.map { case (v, f, _) =>
    (v, f) -> new V2FMessage(v, f)
  }.toMap

  val f2vMsgs: Map[(Factor[V], Var), F2VMessage[V]] = fg.arcs12.map { case (f, v, _) =>
    (f, v) -> new F2VMessage(f, v)
  }.toMap

  def keys1: Set[Factor[V]] = fg.keys1

  def keys2: Set[Var] = fg.keys2

  def outMap12: Map[Factor[V], Set[Var]] = fg.adjMap1

  def outMap21: Map[Var, Set[Factor[V]]] = fg.adjMap2

  def inMap12: Map[Factor[V], Set[Var]] = fg.adjMap1

  def inMap21: Map[Var, Set[Factor[V]]] = fg.adjMap2

  def dataAt12(k1: Factor[V], k2: Var): Option[F2VMessage[V]] = f2vMsgs.get(k1 -> k2)

  def dataAt21(k2: Var, k1: Factor[V]): Option[V2FMessage[V]] = v2fMsgs.get(k2 -> k1)
}

object BeliefPropagationGraph {
  def apply[V](fg: FactorGraph[V])(implicit ct: ClassTag[V]): BeliefPropagationGraph[V] = new BeliefPropagationGraph[V](fg)
}