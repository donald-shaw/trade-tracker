import java.io.StringWriter
import java.nio.file.{FileSystems, Path}
import java.util.Date
import java.text.SimpleDateFormat
import org.shadowlands.tradetracker.model._
import org.shadowlands.tradetracker.processing._
import org.shadowlands.tradetracker.reader._
import org.shadowlands.tradetracker.reporting._

val here = FileSystems.getDefault().getPath(".")
val fs = FileSystems.getDefault.toString
val abs = new java.io.File(".").getAbsolutePath
val root = here.getFileName.getRoot
//val normed = here.getFileName.normalize()
//val parent = here.getParent.getFileName
val base_path_str = "/Users/donaldshaw/Documents/tmp/trading/trade_tracker/data"
val path = FileSystems.getDefault().getPath(base_path_str)
val file0 = path.resolve("test_CS.csv")
val file = path.resolve("ConfirmationDetails_05_to_17-11-16_CS.csv")

val read = readCsv(file)
val outcome = read match {
  case Left(err) => s"Failed to read traces - error: $err"
  case Right(entries) =>
    val events = entries.map(toEvent)
    val traces = accumEvents(events)
    val writer = new StringWriter()
    dump(traces, writer)
    writer.toString
}
