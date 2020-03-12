import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

class Redis {

  var pairsString = HashMap[String, String]()
  var pairList = HashMap[String, ListBuffer[String]]()

  def stringGet(key: String): String = {
    try {
      return pairsString(key)
    } catch {
      case _: Throwable => throw new Exception("key does not exist")
    }
  }

  def listGet(key: String): List[String] = {
    try {
      return pairList(key).toList
    } catch {
      case _: Throwable => throw new Exception("key does not exist")
    }
  }

  def set(key: String, value: String): Int = {
    try {
      val str = stringGet(key)
      val lst = listGet(key)
    } catch {
      case _: Throwable => return 0
    }

    pairsString += (key -> value)
    return 1
  }

  def lpush(key: String, value: String): Int = {
    try {
      var reversed: ListBuffer[String] = listGet(key).reverse.to[ListBuffer]
      reversed += value
      pairList(key) = reversed.reverse
      return 1
    } catch {
      case _: Throwable => pairList += (key -> ListBuffer[String](value)); return 1
    }
  }

  def rpush(key: String, value: String): Int = {
    try {
      var lst: ListBuffer[String] = listGet(key).to[ListBuffer]
      lst += value
      pairList(key) = lst
        return 1
    } catch {
      case _: Throwable => pairList += (key -> ListBuffer[String](value)); return 1
    }
  }

  def addEmptyList(key: String): Int = {
    if (pairList.contains(key) || pairsString.contains(key)) {
      throw new Exception("key already exists")
    }
    pairList += (key -> ListBuffer[String]())
    return 1
  }

  def lpop(key: String): String = {
    try {
      val first = listGet(key)(0)
      listGet(key).drop(1)
      return first
    } catch {
      case _: Throwable => throw new Exception("key does not exist")
    }
  }

  def rpop(key: String): String = {
    try {
      val lst = listGet(key)
      val last = lst(lst.length - 1)
      listGet(key).dropRight(1)
      return last
    } catch {
      case _: Throwable => throw new Exception("key does not exist")
    }
  }

  def lrange(key: String, start: Int, stop: Int): List[String] = {
    try {
      val value = listGet(key)
      var fnl = ListBuffer[String]()
      for( i <- start until stop) {
        fnl += value(i)
      }
      return fnl.toList
    } catch {
      case _: Throwable => throw new Exception("key does not exist")
    }
  }

  def llen(key: String): Int = {
    try {
      return listGet(key).length
    } catch {
      case _: Throwable => throw new Exception("key does not exist")
    }
  }

  def flushall() = {
    pairsString = HashMap[String, String]()
    pairList = HashMap[String, ListBuffer[String]]()
  }

  def getLengthOfLists(): Int = {
    return pairList.size
  }

  def getListKeys(): List[String] = {
    return pairList.keySet.toList
  }

}