package me.yuhuan.gossip.structure

import me.yuhuan.gossip.core._

import scala.reflect.ClassTag

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class ArrayFactor[V: ClassTag](val vars: VarSet) extends Factor[V] { self =>
  val data = Array.ofDim[V](domainSize)

  def apply(configIdx: Int) = data(configIdx)

  def update(configIdx: Int, v: V): Unit = data(configIdx) = v
}

object ArrayFactor {
  def apply[V: ClassTag](vs: Var*)(implicit O: Ordering[Var]): ArrayFactor[V] = {
    new ArrayFactor(VarSet(vs: _*))
  }

  def tabulate[V: ClassTag](vars: Var*)(f: Int => V): ArrayFactor[V] = {
    val φ = ArrayFactor[V](vars: _*)
    for (i <- 0 until φ.domainSize) φ.data(i) = f(i)
    φ
  }

}