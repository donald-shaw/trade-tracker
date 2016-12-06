package org.shadowlands.tradetracker

import java.io.{PrintWriter, Writer}
import java.nio.file.Path
import scala.io.Source

import org.shadowlands.tradetracker.model.{SecurityTrace, Order, Traces}

package object storage {

  private val divider = "===\n"

  def storeOrders(orders: Seq[Order], store: Writer): Unit = {
    orders foreach { order =>
      store.write(s"${order.number}\n")
    }
  }

  def storeTraces(traces: Traces, store: Writer): Unit = {
    traces foreach { case (sec, traces) =>
      store.write(s"${sec.asx_id}: ${traces.map(tr => tr.current).mkString}\n")
    }
  }

  def storeData(orders: Seq[Order], traces: Traces, store: Path): Unit = {
    var writer: Writer = null
    try {
      writer = new PrintWriter(store.toFile)
      storeOrders(orders, writer)
      writer.write(divider)
      storeTraces(traces, writer)
    } catch {
      case ex: Exception => ex.printStackTrace
    } finally {
      if (writer != null) {
        writer.flush()
        writer.close()
      }
    }
  }

  def readOrders(lines: Iterator[String]): Seq[Order] = lines.map(Order).toSeq

  def readTraces(lines: Iterator[String]): Traces = {
    //lines.map(SecurityTrace).toSeq
    Map.empty
  }

  def readData(store: Path): (Seq[Order], Traces) = {
    var source: Source = null
    try {
      source = Source.fromFile(store.toFile)
      val (orders, traces) = source.getLines().span(_ != divider)
      (readOrders(orders), readTraces(traces.drop(1))) // Drop the divider
    } catch {
      case ex: Exception =>
        ex.printStackTrace
        (Nil, Map.empty)
    } finally {
      if (source != null) {
        source.close()
      }
    }
  }
}
