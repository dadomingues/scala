package redfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercicio 1
   */
  def pascal(c: Int, r: Int): Int = {
    // recebe a referência de coluna e linha
    // e retorna qual o valor que esta posição
    // tem no triângulo de Pascal.
    if (c == 0 || r == c || r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercicio 2
   */
  def balance(chars: List[Char]): Boolean = {
    // recebe uma sequencia de caracteres para verificar
    // se os parênteses foram abertos e fechados corretamente
    def iterate(chars: List[Char], qtOpened: Int): Int = {
      if (chars.isEmpty || qtOpened < 0) qtOpened
      else if (chars.head == '(') iterate(chars.tail, qtOpened + 1)
      else if (chars.head == ')') iterate(chars.tail, qtOpened - 1)
      else iterate(chars.tail, qtOpened)
    }
    iterate(chars, 0) == 0
  }

  /**
   * Exercicio 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    // dado valor em dinheiro, calcula o troco
    // conforme moedas disponíveis
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else if (money <= 0 && !coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
  
}
