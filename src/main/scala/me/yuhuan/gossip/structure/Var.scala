package me.yuhuan.gossip.structure

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Var { self =>
  def name: String
  def domainSize: Int


  override def hashCode(): Int = name.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Var => that.name == self.name
    case _ => false
  }


  override def toString: String = s"$name"
}

object Var {

  def apply(_name: String, _domainSize: Int): Var = new Var {
    def name: String = _name
    def domainSize: Int = _domainSize
  }

  implicit object DefaultVarOrdering extends Ordering[Var] {
    //def compare(x: Var, y: Var): Int = x.hashCode - y.hashCode
    def compare(x: Var, y: Var): Int = x.name compare y.name
  }
}
