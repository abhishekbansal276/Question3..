import scala.annotation.tailrec
class ProfitableStock extends App {
  def maxProfit(prices: Array[Int]): Int = {
    if (prices.isEmpty) {
      0
    } else {
      require(prices.forall(_ >= 0), "Input array should contain non-negative integers")
      @tailrec
      def maxProfithlpr(prices: List[Int], maxProfit: Int, lastBuy: Int): Int = {
        prices match {
          case Nil => maxProfit
          case head :: tail => if (head > lastBuy) {
            maxProfithlpr(tail, Math.max(maxProfit, head - lastBuy), lastBuy)
          } else {
            maxProfithlpr(tail, maxProfit, head)
          }
        }
      }
      val listPrices = prices.toList
      maxProfithlpr(listPrices.tail, 0,listPrices.head)
    }
  }
}
