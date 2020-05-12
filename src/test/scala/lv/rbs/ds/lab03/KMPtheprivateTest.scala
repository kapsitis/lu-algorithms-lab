package lv.rbs.ds.lab03

import org.scalatest.Matchers.include
import net.liftweb.json._
import org.scalatest._

object KMPtheprivateTest {

	def getSteps(jsonString:String): List[(Int,Int,Int,Int)] = {
			val json = parse(jsonString)
			implicit val formats = DefaultFormats
			val resultSteps = (json \ "steps").extract[List[Map[String, String]]]
							resultSteps.map {
			step => {
					val theOffset = step("offset").toInt
							val theStart = step("start").toInt
							val theEnd = step("end").toInt
							val containsMatch = step.keySet.contains("match")
							var theMatch = 0
							if (step.keySet.contains("match")) {
								if (step("match").toLowerCase().equals("true")) {
									theMatch = 1
								}
							}
					(theOffset, theStart, theEnd, theMatch)
			}
		}
	}

	/**
	 * Get Prefix table. 
	 * If none is found, return empty list.
	 */
	def getPi(jsonString:String): List[(Int,Int)] = {
			val json = parse(jsonString)
					implicit val formats = DefaultFormats
					val piValues = (json \ "comparisons").extract[List[List[Int]]]
							val prefixFun = piValues.map {
				item => { Tuple2(item(0),item(1)) }
			}
			return prefixFun
	}

	/**
	 * Bad symbol table
	 * If none is found, return empty list.
	 */
	/*
	def getLambda(jsonString:String): List[(Char,Int)] = {
	  val json = parse(jsonString)
    implicit val formats = DefaultFormats
    val gammaValues = (json \ "comparisons").extract[List[List[Int]]]
	  val goodSuffixFun = gammaValues.map {
	    item => { Tuple2(item(0),item(1)) }
	  }
	  return goodSuffixFun	  
	}
	 */

	/**
	 * Good suffix table. 
	 * If none is found, return empty list.
	 */
	def getGamma(jsonString:String): List[(Int,Int)] = {
			val json = parse(jsonString)
					implicit val formats = DefaultFormats
					val gammaValues = (json \ "comparisons").extract[List[List[Int]]]
							val goodSuffixFun = gammaValues.map {
				item => { Tuple2(item(0),item(1)) }
			}
			return goodSuffixFun

	}

	def getComparisons(jsonString:String): Int = {
			val json = parse(jsonString)
					implicit val formats = DefaultFormats
					val comparisons = (json \ "comparisons").extract[Int]
							return comparisons
	}

	def getAlgorithm(jsonString:String): String = {
			val json = parse(jsonString)
					implicit val formats = DefaultFormats
					val algorithm = (json \ "algorithm").extract[String]
							return algorithm
	}
}



class KMPtheprivateTest  extends FunSuite {
  test("Prefix function for abab") {
    val matcher = new KMPmatcher("abab")
    assert(matcher.getPrefixFun.toList === List(-1,0,0,1,2))
  }

  test("Prefix function for aabaab") {
    val matcher = new KMPmatcher("aabaab")
    assert(matcher.getPrefixFun.toList === List(-1,0,1,0,1,2,3))
  }


  test("Prefix function for ABCDABD") {
    val matcher = new KMPmatcher("ABCDABD")
    assert(matcher.getPrefixFun.toList === List(-1,0,0,0,0,1,2,0))
  }

  test("Function findAllIn right at start returns 0") {
    val matcher = new KMPmatcher("ab")
    val result = matcher.findAllIn("abc").toList
    assert(result === List(0))
  }



  test("findAllIn returns 3 items, if they exist") {
    val matcher = new KMPmatcher("ab")
    val result = matcher.findAllIn("stab, about, above").toList
    assert(result === List(2, 6, 13))
  }


  test("Return empty findAllIn iterator, if none exist") {
    val matcher = new KMPmatcher("ab")
    val result = matcher.findAllIn("xxxx, yyyyy, zzzzz").toList
    assert(result === List())
  }

  // **************************************************************************
  // Check if the pattern preprocessing functions are correct
  // **************************************************************************
  test("return prefixFun") {
    val myPattern = "ABCDABD"
    val matcher = new KMPmatcher(myPattern)
    assert(matcher.getPrefixFun === List(-1,0,0,0,0,1,2,0))
  }


  // **************************************************************************
  // Testing the returned JSON as a string.
  // **************************************************************************
  test("Return JSON string with the right algorithm type") {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    assert (result.indexOf("KMP") >= 0)
  }


  test("Return 3 correct string fields") {
    val myPattern = "ABCDABD"
    val matcher = new KMPmatcher(myPattern)
    val myText = "ABC ABCDAB ABCDABCDABDE"
    val result = matcher.toJson(myText)
    val json = parse(result)
    implicit val formats = DefaultFormats
    val aa = (json \ "algorithm").extract[String]
    assert(aa  === "KMP")
    val pp = (json \ "pattern").extract[String]
    assert(pp  === myPattern)
    val tt = (json \ "text").extract[String]
    assert(tt === myText)
  }

  test("Count character comparisons correctly") {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val comparisons = (json \ "comparisons").extract[Int]
    assert(comparisons === 27)
  }

  test("return correct steps 0, 1 and 6") {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val resultSteps = (json \ "steps").extract[List[Map[String, String]]]
    assert(resultSteps(0)("offset") === "0")
    assert(resultSteps(0)("start")  === "0")
    assert(resultSteps(0)("end") === "3")
    assert(resultSteps(0).keySet.contains("match")  === false)

    assert(resultSteps(1)("offset") === "3")
    assert(resultSteps(1)("start") === "0")
    assert(resultSteps(1)("end") === "0")
    assert(resultSteps(1).keySet.contains("match") === false)

    assert(resultSteps(6)("offset") === "15")
    assert(resultSteps(6)("start") === "2")
    assert(resultSteps(6)("end") === "6")
    assert(resultSteps(6)("match") === "true")
  }

  /*
  test("Return all the right steps") {
    val expected = List(
      (0, 0, 3, 0), (3, 0, 0, 0), (4, 0, 6, 0),
      (8, 2, 2, 0), (10, 0, 0, 0), (11, 0, 6, 0),
      (15, 2, 6, 1), (22, 0, 0, 0)
    )
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.findAllSearchSteps("ABC ABCDAB ABCDABCDABDE")
    assert(result === expected)
  }
*/

  // **************************************************************************
  // The following tests are considred "private"
  // **************************************************************************
  test("Prefix function for a 4th order Gray string") {
    val matcher = new KMPmatcher("ABACABADABACABA")
    assert(matcher.getPrefixFun.toList === List(-1,0,0,1,0,1,2,3,0,1,2,3,4,5,6,7))
  }

  test("Prefix function for a Thue-Morse sequence") {
    val matcher = new KMPmatcher("0110100110010110")
    assert(matcher.getPrefixFun.toList === List(-1,0,0,0,1,2,1,1,2,3,4,1,2,1,2,3,4))
  }

  
  // https://youtu.be/V5-7GzOfADQ
  // Abdul Bari. 9.1 Knuth-Morris-Pratt KMP String Matching Algorithm
  test("YouTube sample1 prefix fun") {
    val matcher = new KMPmatcher("abcdf")
    assert(matcher.getPrefixFun.toList === List(-1,0,0,0,0,0))
  }
  
  test("YouTube sample1 findAll") {
    val matcher = new KMPmatcher("abcdf")
    val result = matcher.findAllIn("abcdabcabcdfabcdf").toList
    assert(result === List(7,12))
  }
    
  test("YouTube sample1 steps from JSON") {
    val matcher = new KMPmatcher("abcdf")
    val jsonString = matcher.toJson("abcdabcabcdfabcdf")
    val stepVals = KMPtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,0,4,0), (4,0,3,0), (7,0,4,1), (12,0,4,1))  )
  }
  
  // https://youtu.be/GTJr8OvyEVQ
  // Knuth–Morris–Pratt(KMP) Pattern Matching(Substring search)
  // (Tushar Roy - Coding Made Simple)
  test("YouTube sample2 prefix fun") {
    val matcher = new KMPmatcher("abcdabcy")
    assert(matcher.getPrefixFun.toList === List(-1, 0, 0, 0, 0, 1, 2, 3, 0))
  }
  
  test("YouTube sample2 findAll") {
    val matcher = new KMPmatcher("abcdabcy")
    val result = matcher.findAllIn("abcxabcdabxabcdabcdabcy").toList
    assert(result === List(15))
  }
    
  test("YouTube sample2 steps from JSON") {
    val matcher = new KMPmatcher("abcdabcy")
    val jsonString = matcher.toJson("abcxabcdabxabcdabcdabcy")
    val stepVals = KMPtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,0,3,0), (3,0,0,0), (4,0,6,0), (8,2,2,0), (10,0,0,0), (11,0,7,0), (15,3,7,1)) )
  }
  
  
  // https://youtu.be/BXCEFAzhxGY
  // Knuth–Morris–Pratt (KMP) Pattern Matching Substring Search - First Occurrence Of Substring
  // Back To Back SWE
  test("YouTube sample3 prefix fun") {
    val matcher = new KMPmatcher("dsgwadsgz")
    assert(matcher.getPrefixFun.toList === List(-1, 0, 0, 0, 0, 0, 1, 2, 3, 0))
  }
  
  test("YouTube sample3 findAll") {
    val matcher = new KMPmatcher("dsgwadsgz")
    val result = matcher.findAllIn("adsgwadsxdsgwadsgz").toList
    assert(result === List(9))
  }
    
  test("YouTube sample3 steps from JSON") {
    val matcher = new KMPmatcher("dsgwadsgz")
    val jsonString = matcher.toJson("adsgwadsxdsgwadsgz")
    val stepVals = KMPtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,0,0,0), (1,0,7,0), (6,2,2,0), (8,0,0,0), (9,0,8,1)) )
  }
  
  
  // https://youtu.be/5i7oKodCRJo
  // Knuth–Morris–Pratt algorithm
  // SpookyAlgorithms
  test("YouTube sample4 prefix fun") {
    val matcher = new KMPmatcher("ACACAGT")
    assert(matcher.getPrefixFun.toList ===  	List(-1, 0, 0, 1, 2, 3, 0, 0))
  }
  
  test("YouTube sample4 findAll") {
    val matcher = new KMPmatcher("ACACAGT")
    val result = matcher.findAllIn("ACAT ACGACACAGT").toList
    assert(result === List(8))
  }
    
  test("YouTube sample4 steps from JSON") {
    val matcher = new KMPmatcher("ACACAGT")
    val jsonString = matcher.toJson("ACAT ACGACACAGT")
    
    reflect.io.File("src/test/resources/sample4-kmp-plain.json.txt").writeAll(jsonString)

    
    val stepVals = KMPtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,0,3,0), (2,1,1,0), (3,0,0,0), (4,0,0,0), (5,0,2,0), (7,0,0,0), (8,0,6,1)) )
  }
  
  // https://youtu.be/4jY57Ehc14Y
  // Knuth-Morris-Pratt (KMP) algorithm | String Matching Algorithm | Substring Search
  // Logic First
  test("YouTube sample5 prefix fun") {
    val matcher = new KMPmatcher("onions")
    assert(matcher.getPrefixFun.toList === List(-1, 0, 0, 0, 1, 2, 0))
  }
  
  test("YouTube sample5 findAll") {
    val matcher = new KMPmatcher("onions")
    val result = matcher.findAllIn("onionionspl").toList
    assert(result === List(3))
  }
    
  test("YouTube sample5 steps from JSON") {
    val matcher = new KMPmatcher("onions")
    val jsonString = matcher.toJson("onionionspl")
    val stepVals = KMPtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,0,5,0), (3,2,5,1), (9,0,0,0), (10,0,0,0)) )
  }
  
  
  // https://youtu.be/4Xyhb72LCX4
  // ADS1: Boyer-Moore basics
  // Ben Langmead
  test("YouTube sample6 prefix fun") {
    val matcher = new KMPmatcher("CTTACTTAC")
    assert(matcher.getPrefixFun.toList === List(-1, 0, 0, 0, 0, 1, 2, 3, 4, 5))
  }

  // https://youtu.be/Wj606N0IAsw
  // ADS1: Boyer-Moore: putting it all together
  // Ben Langmead
  test("YouTube sample7 prefix fun") {
    val matcher = new KMPmatcher("GTAGCGGCG")
    assert(matcher.getPrefixFun.toList === List(-1, 0, 0, 0, 1, 0, 1, 1, 0, 1))
  }
  
  
  
}
