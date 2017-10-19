import scala.collection.mutable
import scala.io.Source

object Matcher extends App {
  type Asset = String
  type Client = String
  type Direction = Boolean

  val $ = "$"
  case class OrderKey(asset: Asset, direction: Direction, amount: Int, price: Int)

  def scan(file: String)(op: String => Any) = {
    val in = Source.fromFile(file)
    try {
      in.getLines foreach op
    } finally {
      in.close
    }
  }

  def parseClients(file: String) = {
    val balances = mutable.SortedMap[Client, mutable.SortedMap[Asset, Int]]()
    scan(file) { line =>
      val l = line.split("\\s+")
      val client = l.head
      val assets = Seq($, "A", "B", "C", "D")
      balances += (client -> mutable.SortedMap(assets zip (l.tail map (_.toInt)): _*))
    }
    balances
  }

  def parseOrders(file: String) = {
    val orders = mutable.Map[OrderKey, mutable.Queue[Client]]()
    scan(file) { line =>
      val l = line.split("\\s+")
      val client = l(0)
      val buy = l(1) == "b"
      val asset = l(2)
      val price = l(3).toInt
      val amount = l(4).toInt
      orders get OrderKey(asset, ! buy, amount, price) flatMap { _.dequeueFirst(_ => true) } match {
        case Some(c) =>
          if (c != client) {
            if (buy) execute(client, c, asset, amount, price)
            else execute(c, client, asset, amount, price)
          }
        case None =>
          val key = OrderKey(asset, buy, amount, price)
          orders get key match {
            case Some(queue) => queue += client
            case None => orders += (key -> mutable.Queue(client))
          }
      }
    }
  }

  private def execute(buyer: Client, seller: Client, asset: Asset, amount: Int, price: Int) = {
    balances(buyer)(asset) += amount
    balances(buyer)($) -= amount * price
    balances(seller)(asset) -= amount
    balances(seller)($) += amount * price
  }

  def dumpClients =
    balances.iterator foreach { case (client, assets) =>
      println(client + assets.values.mkString("\t", "\t", ""))
    }

  if (args.length < 2) {
    Console.err.println("Usage: Matcher <clients file> <orders file>")
    System.exit(1)
  }
  val balances = parseClients(args(0))
  parseOrders(args(1))
  dumpClients
}