package org.shadowlands.tradetracker

import java.io.{PrintWriter, Writer}
import java.nio.file.Path
import scala.io.Source

import model.Order

package object storage {

  def storeOrders(orders: Seq[Order], store: Writer): Unit = {
    orders foreach { order =>
      store.write(s"${order.number}\n")
    }
  }

  def storeOrders(orders: Seq[Order], store: Path): Unit = {
    var writer: Writer = null
    try {
      writer = new PrintWriter(store.toFile)
      storeOrders(orders, writer)
    } catch {
      case ex: Exception => ex.printStackTrace
    } finally {
      if (writer != null) {
        writer.flush()
        writer.close()
      }
    }
  }

  def readOrders(store: Source): Seq[Order] = store.getLines().map(Order).toList // NB: toSeq merely hides a stream

  def readOrders(store: Path): Seq[Order] = {
    var source: Source = null
    try {
      source = Source.fromFile(store.toFile)
      readOrders(source)
    } catch {
      case ex: Exception =>
        ex.printStackTrace
        Nil
    } finally {
      if (source != null) {
        source.close()
      }
    }
  }
}
