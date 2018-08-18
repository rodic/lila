package lila.user

import reactivemongo.bson._
import scala.util.Random
import lila.base.LilaException

case class TwoFactorRecoveryCodes(codes: Set[String]) extends AnyVal
class TwoFactorRecoveryCodesError(val message: String) extends LilaException

object TwoFactorRecoveryCodes {

  private val SetSize = 16
  private val CodeLength = 10

  def apply() = new TwoFactorRecoveryCodes(generateCodes())

  def createWithDifferentCodes(codes: Set[String]): TwoFactorRecoveryCodes = {
    val newInstance = TwoFactorRecoveryCodes()
    if ((newInstance.codes & codes) == Set()) newInstance
    else createWithDifferentCodes(codes)
  }

  private def generateCodes(set: Set[String] = Set()): Set[String] = {
    if (set.size == SetSize) set
    else generateCodes(set + generateCode)
  }

  private def generateCode(): String = {
    Random.alphanumeric take CodeLength mkString
  }

  private[user] val twoFactorRecoveryCodesBSONHandler = new BSONHandler[BSONArray, TwoFactorRecoveryCodes] {
    def read(array: BSONArray): TwoFactorRecoveryCodes = {
      TwoFactorRecoveryCodes(array.as[Set[String]])
    }
    def write(recovery: TwoFactorRecoveryCodes): BSONArray = {
      BSONArray(recovery.codes)
    }
  }

  implicit class Presenter(recoveryOption: Option[TwoFactorRecoveryCodes]) {
    private val codes = recoveryOption match {
      case Some(recoveryCodes) => recoveryCodes.codes.toSeq
      case None => Seq()
    }

    def isNotEmpty: Boolean = !codes.isEmpty
    def mapCodes[A](fn: String => A): Seq[A] = codes.map(fn)
  }
}
