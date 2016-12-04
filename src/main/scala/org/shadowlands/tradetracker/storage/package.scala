package org.shadowlands.tradetracker

import java.io.{PrintWriter, Writer}
import java.nio.file.Path

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
}
