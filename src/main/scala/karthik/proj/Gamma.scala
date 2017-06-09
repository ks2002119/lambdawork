package karthik.proj

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

import karthik.proj.NReduceEngine.StringMap
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.Future

/**
  * Created by ksubramanian on 6/7/17.
  */
class Gamma(data : mutable.Map[String, Set[String]]) extends HashMap[String, Set[String]] {
    val futures : ConcurrentMap[String, Future[Expression]] = new ConcurrentHashMap()
    val MAX_LEVEL = 30
    private var recursionCount = 0;
    private var exceeded = false;
    if (data != null) {
        for ((k, v) <- data) {
            put(k, v)
        }
    }
    def this() {
        this(null)
    }
    def inc() = {
        recursionCount = recursionCount + 1;
        if (recursionCount >= MAX_LEVEL) {
            exceeded = true;
        }
    }
    def dec() = {recursionCount = recursionCount - 1;}
    def isExceeded = exceeded
    def +=(kv: (String, String)): Gamma.this.type = super.+=((kv._1, Set(kv._2)))
    def reverse() : StringMap = for ((k,v) <- this; setVal <- v) yield (setVal, k)
    def getFuture(v : String): Future[Expression] = futures.get(v)
    def addFuture(variable : String, future : Future[Expression]) = futures.put(variable, future)
}
