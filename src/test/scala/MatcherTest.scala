import java.io.{PipedInputStream, PipedOutputStream}

import org.scalatest.FunSuite

import scala.io.Source
import scala.collection.mutable

import Matcher._

class MatcherTest extends FunSuite {

  val Empty = "empty.txt"

  test("No orders => no change") {
    check(Empty, Empty, Empty)
    check("mini-clients.txt", Empty, "mini-clients.txt")
  }

  test("No matching orders => no change") {
    check("mini-clients.txt", "no-match.txt", "mini-clients.txt")
  }

  test("No buyers => no change") {
    check("mini-clients.txt", "sells-only.txt", "mini-clients.txt")
  }

  test("No sellers => no change") {
    check("mini-clients.txt", "buys-only.txt", "mini-clients.txt")
  }

  test("Simple two-trade session") {
    check("mini-clients.txt", "mini-orders.txt", "mini-result.txt")
  }

  test("Each order executes once only") {
    check("mini-clients.txt", "one-from-many.txt", "one-from-many-result.txt")
  }

  test("Multiple orders with the same asset/amount/price should all be executed") {
    check("mini-clients.txt", "multi-match.txt", "mini-clients.txt")
  }

  test("Example dataset") {
    val balances = parseClients(pathTo("clients.txt"))
    def execute(buyer: Client, seller: Client, asset: Asset, price: Int, amount: Int) = {
      balances(buyer)(asset) += amount
      balances(buyer)($) -= price * amount
      balances(seller)(asset) -= amount
      balances(seller)($) += price * amount
   }

    val orders = mutable.Queue[(Client, Direction, Asset, Int, Int)]()
    scan(pathTo("orders.txt")) { line =>
      val l = line.split("\\s+")
      val client = l(0)
      val buy = l(1) == "b"
      val asset = l(2)
      val price = l(3).toInt
      val amount = l(4).toInt

      orders dequeueFirst { case (_, b, a, p, amt) =>
        b == !buy && a == asset && p == price && amt == amount
      } match {
        case Some((c, _, _, _, _)) =>
          val (buyer, seller) = if (buy) (client, c) else (c, client)
          execute(buyer, seller, asset, price, amount)
        case None =>
          orders += ((client, buy, asset, price, amount))
      }
    }

    val result = runMatcher("clients.txt", "orders.txt")
    assert(result.size == balances.size)
    balances.iterator.toSeq zip result foreach { case ((client, assets), line) =>
      val l = client + assets.values.mkString("\t", "\t", "")
      assert(l == line)
    }
  }

  private def pathTo(file: String) = "src/test/resources/" + file

  private def runMatcher(clientsFile: String, ordersFile: String) = {
    val opipe = new PipedOutputStream
    val ipipe = new PipedInputStream(opipe)
    Console.withOut(opipe) {
      Matcher.main(Array(pathTo(clientsFile), pathTo(ordersFile)))
    }
    opipe.close()
    Source.fromInputStream(ipipe).getLines.toSeq
  }

  private def check(clientsFile: String, ordersFile: String, goldenFile: String) = {
    val result = runMatcher(clientsFile, ordersFile)
    val golden = Source.fromFile(pathTo(goldenFile)).getLines.toSeq
    assert(result.length == golden.length &&
      (result zip golden forall { case (l1, l2) => l1 == l2 }))
  }
}
