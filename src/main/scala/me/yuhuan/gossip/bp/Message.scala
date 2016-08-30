package me.yuhuan.gossip.bp

import me.yuhuan.gossip._
import me.yuhuan.gossip.core.VarSet
import me.yuhuan.gossip.structure._

import scala.reflect.ClassTag

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class Message[V: ClassTag](val v: Var) extends ArrayFactor[V](VarSet(v))
class V2FMessage[V: ClassTag](v: Var, φ: Factor[V]) extends Message[V](v)
class F2VMessage[V: ClassTag](φ: Factor[V], v: Var) extends Message[V](v)
