package tell.logic

/**
 * Created by nkatz at 4/12/19
 */

object Constant {
  def apply(): Constant = {
    Constant("")
  }
}

case class Constant(override val name: String,
                    plmrk: String = "",
                    override val _type: String = "") extends LogicalExpression {

  //require(!name.toCharArray()(0).isUpper) // Messes with ASP to MLN syntax conversions

  override def tostring = name

  override def tostringQuote = if (plmrk == "-" || plmrk == "#") "\"" + name + "\"" else name

}
