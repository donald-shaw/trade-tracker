package org.shadowlands.tradetracker.main

import java.io.{PrintWriter, StringWriter}

import org.shadowlands.tradetracker.processing._
import org.shadowlands.tradetracker.reader._
import org.shadowlands.tradetracker.reporting._

object TradeTrackerMain {

  def main(args: Array[String]) {
    CliParser.parser.parse(args, CliConfig()) match {
      case Some(config) =>
        System.out.println(config)
        System.exit(runWith(config))
      case None =>
        // arguments are bad, error message will have been displayed
        System.out.println("CLI options are bad (or 'help' requested) - exiting")
        System.exit(0)
    }
  }

  def runWith(cfg: CliConfig) = {
    val writer = cfg.out.map(out => new PrintWriter(out.toFile)).getOrElse(new StringWriter())
    val result = readCsv(cfg.in) match {
      case Left(err) =>
        val err_msg = s"Failed to read traces - error: $err"
        writer.write(err_msg)
        if (cfg.out.isDefined) System.err.append(err_msg)
        1
      case Right(entries) =>
        val events = entries.map(toEvent)
        val traces = accumEvents(events)
        dump(traces, writer)
        0
    }
    writer.close()
    if (cfg.out.isEmpty) {
      System.out.append(writer.toString)
    }
    result
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

    opt[File]('s', "store").optional().valueName("<file>")
      .action( (x, c) => c.copy(store = Some(x.toPath)) )
      .text("out is an optional file property")

//    opt[(String, Int)]("max").action({
//    case ((k, v), c) => c.copy(libName = k, maxCount = v) }).
//    validate( x =>
//    if (x._2 > 0) success
//    else failure("Value <max> must be >0") ).
//    keyValueName("<libname>", "<max>").
//    text("maximum count for <libname>")

//    opt[Seq[Path]]('j', "jars").valueName("<jar1>,<jar2>...").action( (x,c) =>
//    c.copy(jars = x) ).text("jars to include")

//    opt[Map[String,String]]("kwargs").valueName("k1=v1,k2=v2...").action( (x, c) =>
//    c.copy(kwargs = x) ).text("other arguments")

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