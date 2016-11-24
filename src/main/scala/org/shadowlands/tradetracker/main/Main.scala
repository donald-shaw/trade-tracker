package org.shadowlands.tradetracker.main

object TradeTrackerMain {
  val usage = """
    Usage: tracker -f csv_file_name
              """
  def main(args: Array[String]) {
    if (args.length == 0) System.out.println(usage)
    type OptionMap = Map[Symbol, Any]

    def nextOption(map : OptionMap, list: List[String]): OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "--max-size" :: value :: tail =>
          nextOption(map ++ Map('maxsize -> value.toInt), tail)
        case "--min-size" :: value :: tail =>
          nextOption(map ++ Map('minsize -> value.toInt), tail)
        case string :: opt2 :: tail if isSwitch(opt2) =>
          nextOption(map ++ Map('infile -> string), list.tail)
        case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
        case option :: tail => System.out.println("Unknown option "+option)
          System.exit(1)
          Map.empty
      }
    }
    val options = nextOption(Map(), args.toList)
    System.out.println(options)
  }
}