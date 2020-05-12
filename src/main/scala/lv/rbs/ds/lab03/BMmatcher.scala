package lv.rbs.ds.lab03


import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

class BMmatcher(pattern: String) {

  val m = pattern.length

  def getPrefixFun(arg:String): List[Int] = {
    val result = ArrayBuffer.empty[Int]
    for (i <- 0 to m) {
      result += 0
    }
    result(0) = -1
    result(1) = 0
    var k = 0
    for (q <- 2 to m) {
      while (k > 0 && arg.charAt(k) != arg.charAt(q-1)) {
        k = result(k)
      }
      if (arg.charAt(k) == arg.charAt(q-1)) {
        k = k+1
      }
      result(q) = k
    }
    result.toList
  }


  /**
   * This method should return a list of length m (length of the pattern).
   * Good Suffix function
   */
  def getGoodSuffixFun(): List[Int] = {
    //List()
    val result = ArrayBuffer.empty[Int]
    val arg = pattern
    val pi = getPrefixFun(arg)
    //println("pi = " + pi)
    val arg1 = pattern.reverse
    val pi1 = getPrefixFun(arg1)
    //println("pi1 = " + pi1)

    for (j <- 0 to m) {
      result += m-pi(m)
    }

    for (l <- 1 to m) {
      val j = m - pi1(l)
      result(j) = scala.math.min(result(j), l - pi1(l))
    }
    result.toList
  }


  def getBadCharFun(): List[(Char,Int)] = {
    val theMap = getBadCharMap()
    val keys = theMap.keySet.toList.sorted
    for (c <- keys) yield (c, theMap.get(c).get)
  }

  def getBadCharMap(): Map[Char,Int] = {
    val result = Map.empty[Char,Int]
    for (j <- 0 until m) {
      result.put(pattern(j), j)
    }
    return result
  }

  /**
   * Return the value of map (or default 'dd', if not found)
   */
//  def getMapVal(map:Map[Char,Int], c:Char, dd:Int): Int = {
//    if (map.contains(c)) { map.get(c) } else { default }
//  }



  def findAllIn(text: CharSequence): Iterator[Int] = {
    val goodSuffix = getGoodSuffixFun()
    val badCharMap = getBadCharMap()
    val result = ArrayBuffer.empty[Int]
    val n = text.length()
    var s = 0
    while (s <= n-m) {
      var j = m

      while (j > 0 && pattern.charAt(j-1) == text.charAt(s+j-1)) {
        j = j - 1
      }
      if (j == 0) {
        result += s
        s += goodSuffix(0)
      } else {
        s += scala.math.max(goodSuffix(j), j - 1 - badCharMap.getOrElse(text.charAt(s+j-1),-1))
      }
    }
    result.iterator
  }

  def findAllSearchSteps(text: CharSequence): List[(Int,Int,Int,Int)] = {
    val result = ArrayBuffer.empty[(Int,Int,Int,Int)]
    val goodSuffix = getGoodSuffixFun()
    val badCharMap = getBadCharMap()
    val n = text.length()
    var s = 0
    while (s <= n-m) {
      var j = m

      while (j > 0 && pattern.charAt(j-1) == text.charAt(s+j-1)) {
        j = j - 1
      }
      if (j == 0) {
        result += Tuple4(s,m-1,0,1)
        s += goodSuffix(0)
      } else {
        result += Tuple4(s,m-1,j-1,0)
        s += scala.math.max(goodSuffix(j), j - 1 - badCharMap.getOrElse(text.charAt(s+j-1),-1))
      }
    }
    result.toList
  }

  def toJson(text: CharSequence): String = {
    val transcript = new SearchTranscript("BM", pattern, text, findAllSearchSteps(text))
    transcript.getJson()
  }



  def main(args: Array[String]): Unit = {
    val matcher = new BMmatcher("CABAB")
    val gsf = matcher.getGoodSuffixFun()
    //print("gsf = " + gsf)
  }

}
