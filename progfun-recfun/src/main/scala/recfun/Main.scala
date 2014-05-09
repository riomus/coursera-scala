package recfun
import common._

object Main {
  def main(args: Array[String]) {
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c == 0 || r == 0 || c == r) 1 else (pascal(c - 1, r - 1) + pascal(c, r - 1))

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceInner(counter: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty || counter < 0) counter == 0 else chars.head match {
        case '(' => balanceInner(counter + 1, chars.tail)
        case ')' => balanceInner(counter - 1, chars.tail)
        case _ => balanceInner(counter, chars.tail)
      }
    }
    balanceInner(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money,coins) match{
      case (0,_) => 1
      case _ if(money<0)=>0
      case (mon,x::xs)=> countChange(mon,xs)+countChange(mon-x,coins)
      case _ => 0
    }
  }
}