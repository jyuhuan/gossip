package me.yuhuan.gossip.core
import math._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class LogNumber(val logX: Double) extends AnyVal {
  def +(that: LogNumber) = ???
  def -(that: LogNumber) = ???
  def *(that: LogNumber) = ???
  def /(that: LogNumber) = ???
  def value = exp(logX)
  override def toString: String = s"$value"
}
