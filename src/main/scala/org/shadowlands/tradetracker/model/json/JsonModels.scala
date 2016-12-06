package org.shadowlands.tradetracker.model.json

import spray.json.DefaultJsonProtocol._

object JsonEntities {

  case class JsonEvent(conf_num: Int, order_num: String, trade_date: String, action: String, units: Int,
                       security: String, currency: String, ave_price: Double, brokerage: Double, net_proceeds: Double,
                       settle_date: String)
  object JsonEvent { implicit val f = jsonFormat11(JsonEvent.apply) }

  case class JsonTrace(security: String, events: List[JsonEvent], current: Int, resolved: Boolean, currency: String,
                       net_outcome: Double, costs: Double, start: String, end: String, unresolvable: Boolean)
  object JsonTrace { implicit val f = jsonFormat10(JsonTrace.apply) }
}
