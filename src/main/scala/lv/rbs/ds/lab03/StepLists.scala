package lv.rbs.ds.lab03

import net.liftweb.json._
import net.liftweb.json.JsonDSL._

object StepLists {

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

