package module3

import module3.zioConcurrency.{currentTime, printEffectRunningTime}
import module3.zio_homework.config.{AppConfig, Configuration, load}
import zio.{Has, IO, ZIO, ZLayer}
import zio.clock.Clock
import zio.config.ReadError
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.{TypesafeConfig, TypesafeConfigSource}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.File
import scala.language.postfixOps
import scala.util.Try

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  lazy val guessProgram: ZIO[Random with Console, Throwable, Unit] = for {
    value <- getRandomIntValue(1, 3)
    _ <- printText("Ввелите число от 1 до 3")
    input <- getIntInput
    res <- checkNumber(input, value)
  } yield res

  def getRandomIntValue(min: Int, max: Int): ZIO[Random with Console, Throwable, Int] = for {
    random <- ZIO.environment[Random].map(_.get)
    value <- random.nextIntBetween(min, max + 1)
  } yield value

  def getIntInput: ZIO[Console, Throwable, Int] = for {
    console <- ZIO.environment[Console].map(_.get)
    value <- console.getStrLn
    valueInt <- ZIO.fromTry(Try(value.toInt))
  } yield valueInt

  def checkNumber(input: Int, inMemory: Int): ZIO[Console, Throwable, Unit] = for {
    console <- ZIO.environment[Console].map(_.get)
    _ <- console.putStrLn(if (input == inMemory) "Вы угадали нужное число" else "Вы не угадали")
  } yield ()

  def printText(text: String): ZIO[Random with Console, Throwable, Unit] = for {
    console <- ZIO.environment[Console].map(_.get)
    _ <- console.putStrLn(text).orDie
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */
  def doWhile[R, E, A](zio: ZIO[R, E, A])(f: A => Boolean): ZIO[R, E, A] =
    zio.flatMap(a => if (f(a)) doWhile(zio)(f) else ZIO.succeed(a))

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault(fileName: String): ZLayer[Any, ReadError[String], Has[AppConfig]] =
    TypesafeConfig.fromHoconFile(new File(fileName), descriptor[AppConfig]).orElse(Configuration.live)

  def loadConfigOrDefault2(fileName: String): IO[ReadError[String], AppConfig] =
  TypesafeConfigSource.fromHoconFile(new File(fileName)) match {
    case Left(_) => load
    case Right(source) => zio.config.read(descriptor[AppConfig] from source) match {
      case Left(_) => load
      case Right(s) => ZIO.succeed(s)
    }
  }

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Random with Clock with Console, Throwable, Int] = for {
    random <- ZIO.environment[Random].map(_.get)
    _ <- ZIO.sleep(1.second)
    value <- random.nextIntBetween(1, 11)
  } yield value

  /**
   * 4.2 Создайте коллекцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[ZIO[Random with Clock with Console, Throwable, Int]] =
    List(eff,eff,eff,eff,eff,eff,eff,eff,eff,eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: ZIO[Random with Clock with Console, Throwable, List[Int]] = for {
    res <- printEffectRunningTime(ZIO.collectAll(effects))
    _ <- printText(res.mkString(", "))
  } yield res

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */
  //Подробнее? В эффекте eff требование возвращать число спустя одну секунду
  lazy val appSpeedUp: ZIO[Random with Clock with Console with PrintService, Throwable, Iterable[Int]] = for {
    print <- ZIO.environment[PrintService].map(_.get)
    res <- print.printEffectTime(eff.replicateM(10))
    _ <- printText(res.mkString(", "))
  } yield res

  //можно ускорить за счёт асинхронного выполнения
  lazy val appSpeedUpAsync: ZIO[Random with Clock with Console, Throwable, List[Int]] = for {
    res <- printEffectRunningTime(ZIO.collectAllSuccessesPar(effects))
    _ <- printText(res.mkString(", "))
  } yield res


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

    type PrintService = Has[PrintTimeService.Service]

  @accessible
  object PrintTimeService {
    trait Service{
      def printEffectTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A]
    }

    class ServiceImpl extends Service {
      def printEffectTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A] = for {
        start <- currentTime
        r <- zio
        end <- currentTime
        _ <- ZIO.effect(println(s"Running time ${end - start}")).orDie
      } yield r

    }

    val live: ZLayer[Clock, Nothing, PrintService] =
      ZLayer.fromService[Clock.Service, PrintTimeService.Service](_ => new ServiceImpl())

  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */

  //в эффекте 4.3 уже было логирование времени, поэтому оно будет происходить два раза
  lazy val appWithTimeLogg: ZIO[Random with Clock with Console with PrintService, Throwable, List[Int]] = for {
    print <- ZIO.environment[PrintService].map(_.get)
    res <- print.printEffectTime(app)
  } yield res

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  val env: ZLayer[Clock, Throwable, PrintService] = PrintTimeService.live

  type ZEnvPrint = Clock with Console with Random with PrintService

    //???
  /*lazy val runApp = {
    ???
  }*/

}
