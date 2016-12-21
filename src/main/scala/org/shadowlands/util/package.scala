package org.shadowlands

package object util {

  case class AnyMatchKey[K](keys: Set[K]) {

    override def equals(obj: Any): Boolean = obj match {
      case that_amk: AnyMatchKey[K] => keys.intersect(that_amk.keys).nonEmpty
      case _ => false
    }

    // Note: Usual equals/hashCode contract will of necessity be violated by this class, hence,
    // use only with special AnyMatchMap type

    override def canEqual(that: Any): Boolean = that match {
      case that_amk: AnyMatchKey[K] => that_amk.keys.intersect(keys).nonEmpty
      case _ => false
    }

    def matches(key: K) = keys.contains(key)
  }

  class AnyMatchMap[K,V] extends Map[AnyMatchKey[K],V] {

    private val inner: Map[AnyMatchKey[K],V] = Map.empty

    override def +[B1 >: V](kv: (AnyMatchKey[K], B1)) = inner + kv

    override def get(key: AnyMatchKey[K]) = inner.get(key)

    def get(key: K) = inner.filterKeys(_.matches(key)).values

    override def iterator = inner.iterator

    override def -(key: AnyMatchKey[K]) = inner - key
  }

  class AnyMatchListMap[K,V] extends AnyMatchMap[K,Seq[V]] {

    def getList(key: K) = super.get(key).flatten

  }
}
