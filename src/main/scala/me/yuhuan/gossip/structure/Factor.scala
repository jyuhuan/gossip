package me.yuhuan.gossip.structure

import me.yuhuan.algebra.{AdditiveSemigroup, Field, MultiplicativeSemigroup}
import me.yuhuan.gossip.core._

import scala.reflect.ClassTag


/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Factor[V] { self =>

  def vars: VarSet

  def domainSize = vars.domainSize

  def apply(configIdx: Int): V
  def apply(config: Seq[Int]): V = apply(vars.configToConfigIdx(config))
  def update(configIdx: Int, v: V): Unit
  def apply(config: Seq[Int], v: V): Unit = update(vars.configToConfigIdx(config), v)

  def op(that: Factor[V])(f: (V, V) => V)(implicit ct: ClassTag[V]): Factor[V] = {
    //TODO: handle cases of empty var set
    val union = self.vars ⋃ that.vars
    val iter1 = self.vars.configIdcsWrt(union)
    val iter2 = that.vars.configIdcsWrt(union)
    // Eagerly evaluate every product
    val xs = (iter1 zip iter2).map { case (i1, i2) => f(self(i1), that(i2)) }.toSeq
    Factor.tabulate(union: _*)(xs)
  }

  def prod(that: Factor[V])(implicit G: MultiplicativeSemigroup[V], ct: ClassTag[V]): Factor[V] = self.op(that)(G.mul)
  def ⨝(that: Factor[V])(implicit G: MultiplicativeSemigroup[V], ct: ClassTag[V]): Factor[V] = self prod that

  def marg(vs: VarSet)(implicit G: AdditiveSemigroup[V], ct: ClassTag[V]): Factor[V] = {
    val margVars = VarSet(self.vars ⋂ vs: _*)
    val result = ArrayFactor[V](margVars: _*)

    val iter1 = 0 until self.domainSize
    val iter2 = margVars.configIdcsWrt(self.vars)

    for ((i, j) <- iter1 zip iter2) {
      result(j) = G.add(result(j), self(i))
    }
    result
  }
  def ▷(vs: VarSet)(implicit G: AdditiveSemigroup[V], ct: ClassTag[V]): Factor[V] = marg(vs)


  def normalized(implicit F: Field[V], ct: ClassTag[V]): Factor[V] = {
    var sum = F.zero
    for (i <- 0 until self.domainSize) sum = F.add(sum, self(i))
    val result = ArrayFactor.tabulate(self.vars: _*)(i => F.div(self(i), sum))
    result
  }

  def normalizeInplace(implicit F: Field[V]): Unit = {
    var sum = F.zero
    for (i <- 0 until self.domainSize) sum = F.add(sum, self(i))
    for (i <- 0 until self.domainSize) self(i) = F.div(self(i), sum)
  }

  def humanReadableString: String = {
    val sb = new StringBuilder
    sb append self.vars.map(_.name).mkString("\t")
    sb append "\t"
    sb append "Score"
    sb append "\n"
    for (i <- 0 until self.domainSize) {
      val states = self.vars.configIdxToConfig(i)
      sb append states.mkString("\t")
      sb append "\t"
      sb append self(i)
      sb append "\n"
    }
    sb.toString()
  }

  override def toString: String = s"φ_$vars"

}

object Factor {
  def apply[V: ClassTag](vars: Var*): Factor[V] = ArrayFactor[V](vars: _*)

  def tabulate[V: ClassTag](vars: Var*)(f: Int => V): Factor[V] = {
    val φ = ArrayFactor[V](vars: _*)
    for (i <- 0 until φ.domainSize) φ.data(i) = f(i)
    φ
  }

}
