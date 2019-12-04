package oled.logic

/**
 * Created by nkatz at 4/12/19
 */

trait LogicalExpression {

  def tostring: String = ""
  def tostringQuote: String = ""
  def _type: String = ""
  def name: String = ""

  def isVariabe = this match {
    case _: Variable => true
    case _ => false
  }

  def isConstant = this match {
    case _: Constant => true
    case _ => false
  }

}
