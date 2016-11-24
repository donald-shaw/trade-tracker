package org.shadowlands.tradetracker.processing

import java.nio.file.{FileSystems, Path}

case class CliConfig(in: Path = FileSystems.getDefault().getPath("."),
                     out: Path = FileSystems.getDefault().getPath("."),
                     store: Path = FileSystems.getDefault().getPath("."))    // See https://github.com/scopt/scopt
