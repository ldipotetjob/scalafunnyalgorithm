
/**
  *
  * Daily Coding Problem: Problem #4 [Hard]
  * 
  * This problem was asked by Stripe.
  *
  * Given an array of integers, find the first missing positive integer in linear time and constant space.
  * In other words, find the lowest positive integer that does not exist in the array.
  * The array can contain duplicates and negative numbers as well.
  * For example, the input [3, 4, -1, 1] should give 2. The input [1, 2, 0] should give 3.
  *
  * You can modify the input array in-place.
  *
  */

val ainteger = Array(3, 4, -1, 1)

def firstMissingPositive(ainteger: Array[Int]): Int = {
  if (ainteger.size > 0 ) {
    /* filter array + orderIt **/
    val positiveSortedArray = ainteger.filter(_>0).sorted
    val possitiveIndex = positiveSortedArray.indices.map(_+1)
    possitiveIndex.diff(positiveSortedArray).sorted.headOption match {
      case Some(lowestval) => lowestval
      case None =>  possitiveIndex.lastOption match {
        case Some(noneval) => noneval + 1
        case _ =>  1
      }
    }
  } else {
    println("Empty Array !!!!")
    0
  }
}

firstMissingPositive(ainteger) // res13: Int = 2
