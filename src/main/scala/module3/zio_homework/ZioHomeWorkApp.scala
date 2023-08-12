package module3.zio_homework

import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{ExitCode, URIO, ZEnv}

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    appWithTimeLogg.provideSomeLayer[Random with Clock with Console](env).exitCode
}
