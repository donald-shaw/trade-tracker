package org.shadowlands.tradetracker.main

import java.io.{PrintWriter, StringWriter}
import java.nio.file.{FileSystems, Path}

import org.shadowlands.tradetracker.model.{Traces, SecurityTrace, Security}
import org.shadowlands.tradetracker.processing._
import org.shadowlands.tradetracker.reader._
import org.shadowlands.tradetracker.reporting._
import org.shadowlands.tradetracker.storage._

object TradeTrackerMain {

  def main(args: Array[String]) {
    CliParser.parser.parse(args, CliConfig()) match {
      case Some(config) =>
        System.out.println(config)
        System.exit(runWith(config))
      case None =>
        // arguments are bad, error message will have been displayed
        System.out.println("CLI options not valid - exiting")
        System.exit(0)
    }
  }

  def runWith(cfg: CliConfig) = try {
    val (prev_events, prev_traces: Traces) = if (cfg.reset) (Seq.empty, Map.empty) else readData(cfg.store, cfg.debug)
    if (cfg.debug) println(s"\nRead in previously processed events:\n${prev_events.map(_.order.number).mkString(", ")}\n\n")
    if (cfg.debug) println(s"\nRead in previously processed traces:\n${prev_traces.mkString(",\n")}\n\n")
    val writer = cfg.out.map(out => new PrintWriter(out.toFile)).getOrElse(new StringWriter())
    val result = readCsv(cfg.in, TemplateType.fromString(cfg.t_type)) match {
      case Left(err) =>
        val err_msg = s"Failed to read traces - error: $err"
        writer.write(err_msg)
        if (cfg.out.isDefined) System.err.append(err_msg)
        1
      case Right(entries) =>
        val events = entries.map(toEvent).filterNot(entry => prev_events.contains(entry)).sorted
        if (cfg.debug) println(s"Read in new events:\n\n${events.mkString(",\n")}\n\n")
        else if (cfg.codes.nonEmpty) println(s"Read in traces:\n\n${events.filter(ev => cfg.codes.contains(ev.security.asx_id)).mkString(",\n")}\n\n")
        val traces = accumEvents(prev_traces, events, cfg.debug, cfg.codes)
        storeData(prev_events ++ events, traces, cfg.store)
        dump(traces, writer)
        monthlyTotals(traces, writer)
        0
    }
    writer.close()
    if (cfg.out.isEmpty) {
      System.out.append(writer.toString)
    }
    result
  } catch {
    case ex: Exception => ex.printStackTrace()
      2
  }

}

object CliParser {

  import java.io.File
  import scopt._

  val parser = new scopt.OptionParser[CliConfig]("trade-tracker") {

    head("trade-tracker", "0.1.0")

//    opt[Int]('f', "foo").action( (x, c) =>
//    c.copy(foo = x) ).text("foo is an integer property")

    opt[File]('i', "in").required().valueName("<file>")
      .action( (x, c) => c.copy(in = x.toPath) )
      .text("in is a required file property")

    opt[File]('o', "out").optional().valueName("<file>")
      .action( (x, c) => c.copy(out = Some(x.toPath)) )
      .text("out is an optional file property")

    opt[File]('s', "store").valueName("<file>")
      .action( (x, c) => c.copy(store = x.toPath) )
      .text("store is an optional file property")

    opt[String]('t', "type").required().action( (x, c) =>
    c.copy(t_type = x) ).text("type is the template type to use")

//    opt[(String, Int)]("max").action({
//    case ((k, v), c) => c.copy(libName = k, maxCount = v) }).
//    validate( x =>
//    if (x._2 > 0) success
//    else failure("Value <max> must be >0") ).
//    keyValueName("<libname>", "<max>").
//    text("maximum count for <libname>")

    opt[Seq[String]]('c', "codes").valueName("<code1>,<code2>...").action( (x,c) =>
    c.copy(codes = x) ).text("codes to debug")

//    opt[Map[String,String]]("kwargs").valueName("k1=v1,k2=v2...").action( (x, c) =>
//    c.copy(kwargs = x) ).text("other arguments")

    opt[Unit]("reset")
      .action( (_, c) => c.copy(reset = true) )
      .text("reset is a flag")

    opt[Unit]("verbose")
      .action( (_, c) => c.copy(verbose = true) )
      .text("verbose is a flag")

    opt[Unit]("debug").hidden()
      .action( (_, c) => c.copy(debug = true) )
      .text("this option is hidden in the usage text")

    help("help").text("prints this usage text")

//    arg[Path]("<file>...").unbounded().optional().action( (x, c) =>
//    c.copy(files = c.files :+ x) ).text("optional unbounded args")

    //note("some notes.".newline)
    note("some notes.\n")

//    cmd("update")
//      .action( (_, c) => c.copy(mode = "update") )
//      .text("update is a command.")
//      .children(
//        opt[Unit]("not-keepalive").abbr("nk")
//          .action( (_, c) => c.copy(keepalive = false) )
//          .text("disable keepalive"),
//        opt[Boolean]("xyz")
//          .action( (x, c) => c.copy(xyz = x) )
//          .text("xyz is a boolean property"),
//        opt[Unit]("debug-update").hidden()
//          .action( (_, c) => c.copy(debug = true) )
//          .text("this option is hidden in the usage text"),
//        checkConfig( c => if (c.keepalive && c.xyz) failure("xyz cannot keep alive") else success )
//      )
  }

}

case class CliConfig(in: Path = FileSystems.getDefault().getPath("."),
                     out: Option[Path] = None, //FileSystems.getDefault().getPath("."),
                     store: Path = FileSystems.getDefault().getPath("./data/tt_store"),
                     t_type: String = "",
                     reset: Boolean = false,
                     verbose: Boolean = false,
                     debug: Boolean = false,
                     codes: Seq[String] = Nil)    // See https://github.com/scopt/scopt
