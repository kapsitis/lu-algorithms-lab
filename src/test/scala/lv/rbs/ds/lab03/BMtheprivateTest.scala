package lv.rbs.ds.lab03

import org.scalatest._
import net.liftweb.json._
import org.scalatest.Matchers._

// sbt
// testOnly *theprivateTest 
object BMtheprivateTest {

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



class BMtheprivateTest  extends FunSuite {
  test("good suffix in CABAB") {
    val matcher = new BMmatcher("CABAB")
    val result = matcher.getGoodSuffixFun
    assert(result === List(5,5,5,2,2,1))
  }

  test("good suffix in ABBAB") {
    val matcher = new BMmatcher("ABBAB")
    val result = matcher.getGoodSuffixFun
    assert(result === List(3,3,3,3,2,1))
  }

  test("good suffix in CBAAB") {
    val matcher = new BMmatcher("CBAAB")
    val result = matcher.getGoodSuffixFun
    assert(result === List(5,5,5,5,3,1))
  }

  test("return 3 items in findAllIn iterator, if they exist") {
    val matcher = new BMmatcher("ab")
    val result = matcher.findAllIn("stab, about, above").toList
    assert(result === List(2, 6, 13))
  }

  test("Bad char function in CABAB") {
    val matcher = new BMmatcher("CABAB")
    val result = matcher.getBadCharFun
    assert(result === List(('A',3),('B',4),('C',0)))
  }




  test("Return empty findAllIn iterator, if none exist") {
    val matcher = new BMmatcher("ab")
    val result = matcher.findAllIn("xxxx, yyyyy, zzzzz").toList
    assert(result === List())
  }

  test("Return 1 item in findAllIn iterator") {
    val matcher = new BMmatcher("ABBABAB")
    val result = matcher.findAllIn("ABAABABBBBABBBAAABBABAB").toList
    assert(result === List(16))
  }


  // **************************************************************************
  // Check if the pattern preprocessing functions are correct
  // **************************************************************************
  test("Return goodSuffixFun for ABCDABD") {
    val expected = List(7,7,7,7,7,7,3,1)
    val myPattern = "ABCDABD"
    val matcher = new BMmatcher(myPattern)
    assert(matcher.getGoodSuffixFun === expected)
  }

  test("Return badCharacterFun") {
    val expected:List[(Char,Int)] = List(('A',4),('B',5),('C',2),('D',6))
    val myPattern = "ABCDABD"
    val matcher = new BMmatcher(myPattern)
    assert(matcher.getBadCharFun === expected)
  }


  // **************************************************************************
  // Testing the returned JSON as a string.
  // **************************************************************************
  test("Return JSON string with the right algorithm type") {
    val matcher = new BMmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    assert(result.indexOf("BM") >= 0)
  }

  // **************************************************************************
  // Testing the returned JSON as a parsed data structure.
  // **************************************************************************
  test("Return 3 correct string fields") {
    val myPattern = "ABCDABD"
    val matcher = new BMmatcher(myPattern)
    val myText = "ABC ABCDAB ABCDABCDABDE"
    val result = matcher.toJson(myText)
    val json = parse(result)
    implicit val formats = DefaultFormats
    val aa = (json \ "algorithm").extract[String]
    assert(aa === "BM")
    val pp = (json \ "pattern").extract[String]
    assert(pp === myPattern)
    val tt = (json \ "text").extract[String]
    assert(tt === myText)
  }

  test ("Return correct steps 0, 1 and 3") {
    val matcher = new BMmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val resultSteps = (json \ "steps").extract[List[Map[String, String]]]
    assert(resultSteps(0)("offset") === "0")
    assert(resultSteps(0)("start") === "6")
    assert(resultSteps(0)("end") === "6")
    assert(resultSteps(0).keySet.contains("match") === false)

    assert(resultSteps(1)("offset") === "4")
    assert(resultSteps(1)("start") === "6")
    assert(resultSteps(1)("end")  === "6")
    assert(resultSteps(1).keySet.contains("match") === false)



    assert(resultSteps(2)("offset") === "11")
    assert(resultSteps(2)("start") === "6")
    assert(resultSteps(2)("end") === "6")
    assert(resultSteps(2).keySet.contains("match") === false)


    assert(resultSteps(3)("offset") === "15")
    assert(resultSteps(3)("start") === "6")
    assert(resultSteps(3)("end") === "0")
    assert(resultSteps(3)("match") === "true")
  }

  /*
  test("Return all the right steps") {
    val expected = List(
      (0, 6, 6, 0), (4, 6, 6, 0), (11, 6, 6, 0),
      (15, 6, 0, 1)
    )
    val matcher = new BMmatcher("ABCDABD")
    val result = matcher.findAllSearchSteps("ABC ABCDAB ABCDABCDABDE")
    assert(result === expected)
  }
  */

  test("Bad character function for a 4th order Gray string") {
    val matcher = new BMmatcher("ABACABADABACABA")
    assert(matcher.getBadCharFun === List(('A',14), ('B',13), ('C',11), ('D',7)))
  }

  test("Bad character function for a Thue-Morse sequence") {
    val matcher = new BMmatcher("0110100110010110")
    assert(matcher.getBadCharFun === List(('0',15), ('1',14)))
  }

  test("Good suffix function for a 4th order Gray string") {
    val matcher = new BMmatcher("ABACABADABACABA")
    assert(matcher.getGoodSuffixFun === List(8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 4, 2, 1))
  }

  test("Good suffix function for a Thue-Morse sequence") {
    val matcher = new BMmatcher("0110100110010110")
    assert(matcher.getGoodSuffixFun === List(12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 6, 6, 3, 3, 1))
  }


  // https://youtu.be/V5-7GzOfADQ
  // Abdul Bari. 9.1 Knuth-Morris-Pratt KMP String Matching Algorithm
  test("YouTubeBoyer sample1 prefix fun") {
    val matcher = new BMmatcher("abcdf")
    assert(matcher.getGoodSuffixFun.toList === List(5, 5, 5, 5, 5, 1) )
  }
  
  test("YouTubeBoyer sample1 findAll") {
    val matcher = new BMmatcher("abcdf")
    val result = matcher.findAllIn("abcdabcabcdfabcdf").toList
    assert(result === List(7,12))
  }
    
  test("YouTubeBoyer sample1 steps from JSON") {
    val matcher = new BMmatcher("abcdf")
    val jsonString = matcher.toJson("abcdabcabcdfabcdf")
    val stepVals = BMtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,4,4,0), (4,4,4,0), (7,4,0,1), (12,4,0,1))  )
  }
  
  // https://youtu.be/GTJr8OvyEVQ
  // Knuth–Morris–Pratt(KMP) Pattern Matching(Substring search)
  // (Tushar Roy - Coding Made Simple)
  test("YouTubeBoyer sample2 prefix fun") {
    val matcher = new BMmatcher("abcdabcy")
    assert(matcher.getGoodSuffixFun.toList === List(8, 8, 8, 8, 8, 8, 8, 8, 1))
  }
  
  test("YouTubeBoyer sample2 findAll") {
    val matcher = new BMmatcher("abcdabcy")
    val result = matcher.findAllIn("abcxabcdabxabcdabcdabcy").toList
    assert(result === List(15))
  }
    
  test("YouTubeBoyer sample2 steps from JSON") {
    val matcher = new BMmatcher("abcdabcy")
    val jsonString = matcher.toJson("abcxabcdabxabcdabcdabcy")
    val stepVals = BMtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,7,7,0), (4,7,7,0), (7,7,7,0), (11,7,7,0), (15,7,0,1)) )
  }
  
  
  // https://youtu.be/BXCEFAzhxGY
  // Knuth–Morris–Pratt (KMP) Pattern Matching Substring Search - First Occurrence Of Substring
  // Back To Back SWE
  test("YouTubeBoyer sample3 prefix fun") {
    val matcher = new BMmatcher("dsgwadsgz")
    assert(matcher.getGoodSuffixFun.toList === List(9, 9, 9, 9, 9, 9, 9, 9, 9, 1))
  }
  
  test("YouTubeBoyer sample3 findAll") {
    val matcher = new BMmatcher("dsgwadsgz")
    val result = matcher.findAllIn("adsgwadsxdsgwadsgz").toList
    assert(result === List(9))
  }
    
  test("YouTubeBoyer sample3 steps from JSON") {
    val matcher = new BMmatcher("dsgwadsgz")
    val jsonString = matcher.toJson("adsgwadsxdsgwadsgz")
    val stepVals = BMtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,8,8,0), (9,8,0,1)) )
  }
  
  
  // https://youtu.be/5i7oKodCRJo
  // Knuth–Morris–Pratt algorithm
  // SpookyAlgorithms
  test("YouTube sample4 prefix fun") {
    val matcher = new BMmatcher("ACACAGT")
    assert(matcher.getGoodSuffixFun.toList === List(7, 7, 7, 7, 7, 7, 7, 1))
  }
  
  test("YouTube sample4 findAll") {
    val matcher = new BMmatcher("ACACAGT")
    val result = matcher.findAllIn("ACAT ACGACACAGT").toList
    assert(result === List(8))
  }
    
  test("YouTube sample4 steps from JSON") {
    val matcher = new BMmatcher("ACACAGT")
    val jsonString = matcher.toJson("ACAT ACGACACAGT")
    val stepVals = BMtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,6,6,0), (3,6,6,0), (6,6,6,0), (8,6,0,1))  )
  }
  
  // https://youtu.be/4jY57Ehc14Y
  // Knuth-Morris-Pratt (KMP) algorithm | String Matching Algorithm | Substring Search
  // Logic First
  test("YouTubeBoyer sample5 prefix fun") {
    val matcher = new BMmatcher("onions")
    assert(matcher.getGoodSuffixFun.toList === List(6, 6, 6, 6, 6, 6, 1))
  }
  
  test("YouTubeBoyer sample5 findAll") {
    val matcher = new BMmatcher("onions")
    val result = matcher.findAllIn("onionionspl").toList
    assert(result === List(3))
  }
    
  test("YouTubeBoyer sample5 steps from JSON") {
    val matcher = new BMmatcher("onions")
    val jsonString = matcher.toJson("onionionspl")
    val stepVals = BMtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,5,5,0), (3,5,0,1)) )
  }
  
  
  // https://youtu.be/4Xyhb72LCX4
  // ADS1: Boyer-Moore basics
  // Ben Langmead
  test("YouTubeBoyer sample6 prefix fun") {
    val matcher = new BMmatcher("CTTACTTAC")
    assert(matcher.getGoodSuffixFun.toList === List(4, 4, 4, 4, 4, 4, 4, 4, 4, 1))
  }
  
  test("YouTubeBoyer sample6 findAll") {
    val matcher = new BMmatcher("CTTACTTAC")
    val result = matcher.findAllIn("CGTGCCTACTTACTTACTTACTTACGCGAA").toList
    assert(result === List(8, 12, 16))
  }
    
  test("YouTubeBoyer sample6 steps from JSON") {
    val matcher = new BMmatcher("CTTACTTAC")
    val jsonString = matcher.toJson("CGTGCCTACTTACTTACTTACTTACGCGAA")
    val stepVals = BMtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,8,5,0), (4,8,1,0), (8,8,0,1), (12,8,0,1), (16,8,0,1), (20,8,8,0), (21,8,8,0)))
  }
  
  // https://youtu.be/Wj606N0IAsw
  // ADS1: Boyer-Moore: putting it all together
  // Ben Langmead
  test("YouTubeBoyer sample7 prefix fun") {
    val matcher = new BMmatcher("GTAGCGGCG")
    assert(matcher.getGoodSuffixFun.toList === List(8, 8, 8, 8, 8, 8, 3, 3, 2, 1))
  }
  
  test("YouTubeBoyer sample7 findAll") {
    val matcher = new BMmatcher("GTAGCGGCG")
    val result = matcher.findAllIn("GTTATAGCTGATCGCGGCGTAGCGGCGAA").toList
    assert(result ===  	List(18))
  }
    
  test("YouTubeBoyer sample7 steps from JSON") {
    val matcher = new BMmatcher("GTAGCGGCG")
    val jsonString = matcher.toJson("GTTATAGCTGATCGCGGCGTAGCGGCGAA")
        
    reflect.io.File("src/test/resources/sample7-bm.json.txt").writeAll(jsonString)
    
    val stepVals = BMtheprivateTest.getSteps(jsonString)
    assert(stepVals === List((0,8,8,0), (7,8,5,0), (10,8,2,0), (18,8,0,1)))
  }
  
  
  // https://youtu.be/G-h1Dph9IOE
  // How to match pattern in string - Naive Method and Boyer Moore Method explained | Team MAST
  // Team MAST
  test("YouTubeBoyer sample8 prefix fun") {
    val matcher = new BMmatcher("TEAMMAST")
    assert(matcher.getGoodSuffixFun.toList === List(7, 7, 7, 7, 7, 7, 7, 7, 1))
  }
  
  test("YouTubeBoyer sample8 findAll") {
    val matcher = new BMmatcher("TEAMMAST")
    val result = matcher.findAllIn("WELCOMETOTEAMMAST").toList
    assert(result ===  	List(9))
  }
    
  test("YouTubeBoyer sample8 steps from JSON") {
    val matcher = new BMmatcher("TEAMMAST")
    val jsonString = matcher.toJson("WELCOMETOTEAMMAST")
    val stepVals = BMtheprivateTest.getSteps(jsonString)
    assert(stepVals ===  	List((0,7,6,0), (7,7,7,0), (9,7,0,1)) )
  }
  
}
