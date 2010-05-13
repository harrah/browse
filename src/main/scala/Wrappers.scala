/* sxr -- Scala X-Ray
 * Copyright 2009  Mark Harrah
 */
package sxr.wrap
import sxr._

// This file exists for compatibility between Scala 2.7.x and 2.8.0

import java.util.{Map => JMap, Set => JSet, SortedSet => JSortedSet}

private[sxr] object Wrappers
{
	def javaMap[K,V](pairs: (K,V)*) =
	{
		val basic = basicMap[K,V]
		for( (k,v) <- pairs)
			basic(k) = v
		basic.underlying
	}
	def basicMap[K,V] = new MutableMapWrapper(new java.util.HashMap[K,V])
	def identityMap[K,V] = new MutableMapWrapper(new java.util.IdentityHashMap[K,V])
	def weakMap[K,V] = new MutableMapWrapper(new java.util.WeakHashMap[K,V])
	def treeSet[V <: Comparable[V]] = new SortedSetWrapper(new java.util.TreeSet[V])
	def toList[K,V](s: java.util.Map[K,V]): List[(K,V)] = toList(s.entrySet).map(e => (e.getKey, e.getValue))
	def toList[T](s: java.util.Collection[T]): List[T] = toList(s.iterator)
	def toList[T](s: java.util.Iterator[T]): List[T] =
	{
		def add(l: List[T]): List[T] =
			if(s.hasNext)
				add(s.next() :: l)
			else
				l
		add(Nil).reverse
	}
	def toList[T](s: java.util.Enumeration[T]): List[T] =
	{
		def add(l: List[T]): List[T] =
			if(s.hasMoreElements)
				add(s.nextElement() :: l)
			else
				l
		add(Nil).reverse
	}
	def readOnly[K,V](map: scala.collection.mutable.Map[K,V]): scala.collection.Map[K,V] = map//.readOnly
	def readOnly[T](set: scala.collection.mutable.Set[T]): scala.collection.Set[T] = set//.readOnly
	def readOnly[T](buffer: scala.collection.mutable.Buffer[T]): Seq[T] = buffer//.readOnly
}

private[sxr] sealed abstract class Iterable[T]
{
	def foreach(f: T => Unit) = toList.foreach(f)
	def toList: List[T]
}
private[sxr] sealed trait Removable[T]
{
	def -=(t: T) : Unit
	def --=(all: Iterable[T]) { all.foreach(-=) }
	def --=(all: scala.Iterable[T]) { all.foreach(-=) }
}
private[sxr] sealed trait Addable[T]
{
	def +=(t: T) : Unit
	def ++=(all: Iterable[T]) { all.foreach(+=) }
	def ++=(all: scala.Iterable[T]) { all.foreach(+=) }
}
private[sxr] sealed abstract class Set[T] extends Iterable[T]
{
	def contains(t: T): Boolean
	def isEmpty: Boolean
}
private[sxr] sealed class SetWrapper[T](val underlying: JSet[T]) extends Set[T]
{
	def isEmpty = underlying.isEmpty
	def contains(t: T) = underlying.contains(t)
	def toList =Wrappers.toList(underlying.iterator)
}
private[sxr] sealed class MutableSetWrapper[T](wrapped: JSet[T]) extends SetWrapper[T](wrapped) with Addable[T] with Removable[T]
{
	def +=(t: T) { underlying.add(t) }
	def -=(t: T) { underlying.remove(t) }
	def readOnly: Set[T] = this
}
private[sxr] final class SortedSetWrapper[T](wrapped: JSortedSet[T]) extends MutableSetWrapper(wrapped)
{
	def first: Option[T] = if(isEmpty) None else Some(wrapped.first)
	def last: Option[T] = if(isEmpty) None else Some(wrapped.last)
	def range(from: T, to: T): SortedSetWrapper[T] = new SortedSetWrapper(wrapped.subSet(from, to))
}
private[sxr] sealed abstract class Map[K,V] extends Iterable[(K,V)]
{
	def apply(key: K): V
	def get(key: K): Option[V]
	final def getOrElse[V2 >: V](key: K, default: => V2): V2 =
		get(key) match
		{
			case Some(value) => value
			case None => default
		}
}
private[sxr] sealed abstract class MapWrapper[K,V](val underlying: JMap[K,V]) extends Map[K,V]
{
	final def apply(key: K) = underlying.get(key)
	final def get(key: K) =
	{
		val value = underlying.get(key)
		if(value == null)
			None
		else
			Some(value)
	}
	final def toList = Wrappers.toList(underlying)
	final def values = toList.map(_._2)
}
private[sxr] sealed class MutableMapWrapper[K,V](wrapped: JMap[K,V]) extends MapWrapper[K,V](wrapped) with Removable[K] with Addable[(K,V)]
{
	final def getOrElseUpdate(key: K, default: => V): V =
		get(key) match
		{
			case Some(value) => value
			case None =>
				val newValue = default
				underlying.put(key, newValue)
				newValue
		}
	final def clear() = underlying.clear()
	final def update(key: K, value: V) { underlying.put(key, value) }
	final def +=(pair: (K, V) ) { update(pair._1, pair._2) }
	final def -=(key: K) { underlying.remove(key) }
	final def readOnly: Map[K,V] = this
}