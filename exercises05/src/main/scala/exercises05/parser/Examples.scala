package exercises05.parser

import exercises05.either.EitherCombinators._
import Error._
import exercises05.either.EitherCombinators

import scala.util.matching.Regex

case class CheckPassportResult(series: Long, number: Long, isSuccess: Boolean)

object Examples {

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
    * если rawUser.id не парсится в Long то функция должна вернуть None
    * если rawUser.banned, то вернуть None
    * используйте for-comprehension
    */
  def transformToOption(rawUser: RawUser): Option[User] =
    for {
      banned     <- if (rawUser.banned) None else Some(rawUser.banned)
      id         <- rawUser.id.toLongOption
      firstName  <- rawUser.firstName
      secondName <- rawUser.secondName
      passport   <- checkPassport(rawUser.passport)
    } yield
      if (passport.isSuccess)
        User(id, UserName(firstName, secondName, rawUser.thirdName), Some(Passport(passport.series, passport.number)))
      else User(id, UserName(firstName, secondName, rawUser.thirdName), None)

  private val correctPattern: Regex = """(\d+) (\d+)""".r

  private def checkPassport(str: Option[String]): Option[CheckPassportResult] = str match {
    case Some(value) =>
      value match {
        case correctPattern(series, number) =>
          Some(
            CheckPassportResult(series.toLongOption.getOrElse(0), number.toLongOption.getOrElse(0), isSuccess = true)
          )
        case _ => None
      }
    case None => Some(CheckPassportResult(0, 0, isSuccess = false))
  }

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
    * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
    * если rawUser.banned, то вернуть Left(Banned)
    * у ошибок есть приоритет:
    * 1. Banned
    * 2. InvalidId
    * 3. InvalidName
    * 4. InvalidPassport
    * используйте for-comprehension
    * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
    */
//  def transformToEither(rawUser: RawUser): Either[Error, User] = {
//    val passportOption = checkPassport(rawUser.passport)
//    (for {
//      id <- rawUser.id.toLongOption
//      firstName <- rawUser.firstName
//      secondName <- rawUser.secondName
//      passport <- passportOption
//      if !rawUser.banned
//    } yield (id, firstName, secondName, passport)) match {
//      case Some((id, firstName, secondName, CheckPassportResult(series, number, true))) =>
//        Right(User(id, UserName(firstName, secondName, rawUser.thirdName), Some(Passport(series, number))))
//      case Some((id, firstName, secondName, CheckPassportResult(_, _, false))) =>
//        Right(User(id, UserName(firstName, secondName, rawUser.thirdName), None))
//      case None =>
//        (rawUser.banned, rawUser.id.toLongOption, rawUser.firstName, rawUser.secondName, passportOption) match {
//          case (true, _, _, _, _) => Left(Banned)
//          case (_, None, _, _, _) => Left(InvalidId)
//          case (_, _, None, _, _) => Left(InvalidName)
//          case (_, _, _, None, _) => Left(InvalidName)
//          case (_, _, _, _, None) => Left(InvalidPassport)
//        }
//    }
//  }
  private def generateUser(
      id: Long,
      firstName: String,
      secondName: String,
      thirdName: Option[String],
      passport: Option[Passport]
  ): Either[Error, User] = {
    Right(User(id, UserName(firstName, secondName, thirdName), passport))
  }

  def transformToEither(rawUser: RawUser): Either[Error, User] =
    for {
      _          <- if (rawUser.banned) Left(Banned) else Right()
      id         <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
      firstName  <- Either.fromOption(rawUser.firstName)(InvalidName)
      secondName <- Either.fromOption(rawUser.secondName)(InvalidName)
      passport   <- Either.fromOption(checkPassport(rawUser.passport))(InvalidPassport)
      user <- if (passport.isSuccess)
        generateUser(id, firstName, secondName, rawUser.thirdName, Some(Passport(passport.series, passport.number)))
      else generateUser(id, firstName, secondName, rawUser.thirdName, None)
      finalResult <- Right(user)
    } yield finalResult
}
