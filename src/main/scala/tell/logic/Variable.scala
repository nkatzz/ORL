package tell.logic

/**
 * Created by nkatz at 4/12/19
 */

case class Variable(override val name: String,
                    inOrOutVar: String = "",
                    override val _type: String = "") extends LogicalExpression {

  require(name.toCharArray()(0).isUpper)

  override def tostring = name

  override def tostringQuote = if (inOrOutVar == "-" || inOrOutVar == "#") "\"" + name + "\"" else name

}

