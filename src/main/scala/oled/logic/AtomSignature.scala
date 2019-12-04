package oled.logic

/**
 * Created by nkatz on 7/10/19.
 */

class AtomSignature(val predSymbol: String, arity: Int) {

  def tostring = s"$predSymbol/$arity"

}
