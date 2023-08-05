package catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet.{WalletError, _}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.io.Source
import scala.util.Try

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_] : Sync](id: WalletId) extends Wallet[F] {
  def balance: F[BigDecimal] = readFile(id)

  def topup(amount: BigDecimal): F[Unit] = for {
    value <- readFile(id)
    res <- writeToFile(id, value + amount)
  } yield res

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] =
    for {
      balance <- readFile(id)
      dif <- Sync[F].delay(balance - amount)
      res <- if (dif > 0) writeToFile(id, dif).map(Right(_)) else Sync[F].delay(Left(BalanceTooLow))
    } yield res

}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = for {
    _ <- createNewFileIfNeeded(id)
    res <- Sync[F].delay(new FileWallet(id))
  } yield res

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
  case class UnexpectedError(message: String) extends WalletError

  def writeToFile[F[_]: Sync](id: WalletId, contents: BigDecimal): F[Unit] = Sync[F].blocking(Try(
      Files.write(Paths.get(id),
        contents.toString().getBytes(StandardCharsets.UTF_8), StandardOpenOption.SYNC)
  ))

  def readFile[F[_] : Sync](id: WalletId): F[BigDecimal] = {
    Sync[F].bracket(
      Sync[F].blocking(Source.fromFile(id))) { s =>
      Sync[F].delay(BigDecimal(s.getLines.mkString))
    } { s => Sync[F].delay(s.close()) }
  }

  private def createNewFileIfNeeded[F[_] : Sync](id: WalletId): F[Unit] =
    if (!Files.exists(Paths.get(id))) {
      for {
        _ <- Sync[F].delay(new File(id).createNewFile())
        res <- writeToFile(id, BigDecimal(0))
      } yield res
    } else Sync[F].unit

}
