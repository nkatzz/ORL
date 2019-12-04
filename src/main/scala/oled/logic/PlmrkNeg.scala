package oled.logic

/**
 * Created by nkatz at 4/12/19
 */

case class PlmrkNeg(override val _type: String) extends LogicalExpression {

  override val tostring = "-" + _type
  override def tostringQuote = this.tostring

}
