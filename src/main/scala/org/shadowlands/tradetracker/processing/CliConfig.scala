package org.shadowlands.tradetracker.processing

import java.nio.file.{FileSystems, Path}

case class CliConfig(in: Path = FileSystems.getDefault().getPath("."),
                     out: Option[Path] = None, //FileSystems.getDefault().getPath("."),
                     store: Option[Path] = None, //FileSystems.getDefault().getPath("."),
                     verbose: Boolean = false,
                     debug: Boolean = false)    // See https://github.com/scopt/scopt
