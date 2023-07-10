package scala3_2

import scala3_2.homework2.CompletionArg
import scala3_2.homework3.Logarithm

object homework1 {
  extension (x: String)
    def +(y: String): Int = {
      s"$x$y".toInt
    }

  extension (x: String)
    def +(y: Int): Int = {
      (x + y.toString).toInt
    }

  extension (x: String)
    def ++(y: String): Int = {
      s"$x$y".toInt
    }

  extension (x: Int)
    def +(y: String): Int = {
      (x.toString + y).toInt
    }


  @main def part1Ex(): Unit ={
    val t: String = ""
    println("1" + "33") // вызывается стандартный метод String
    println("1" + 33) // вызывается стандартный метод String, мсжно закомментировать extension
    println(("1" +"33").isInstanceOf[Int]) //не работает, + не переопределяется
    println("1".+("33").isInstanceOf[Int]) //false
    println(("1" + "33").isInstanceOf[Int]) //false
    println((1 + "33").isInstanceOf[Int]) //автоконвертация, extension игнорируется
    println(("1" ++ "33").isInstanceOf[Int]) //true, extension сработал
  }
}

object homework2 {

  enum CompletionArg:
    case Error(s: String)
    case Response(f: Float)
    case Status(code: Int)

  object CompletionArg {
    given fromString: Conversion[String, CompletionArg] = CompletionArg.Error(_)

    given fromInt: Conversion[Int, CompletionArg] = CompletionArg.Status(_)

    given fromFloat: Conversion[Float, CompletionArg] = CompletionArg.Response(_)
  }

  @main def part2Ex(): Unit ={
    println(Completions.complete("String"))
    println(Completions.complete(1))
    println(Completions.complete(7f))
  }
}

object Completions {

  def complete(arg: CompletionArg): String = arg match {
    case CompletionArg.Error(s) => s"error: $s"
    case CompletionArg.Response(f) => s"response: $f"
    case CompletionArg.Status(code) => s"status: $code"
  }
}


object homework3 {
  opaque type Logarithm = Double

  object Logarithm{
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if d > 0.0 then Some(math.log(d)) else None
  }

  extension (x: Logarithm)
    def toDouble: Double = math.exp(x)
    def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def * (y: Logarithm): Logarithm = x + y

  @main def part3Ex(): Unit ={
    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2
    println(l)
    println(l2)
    println(l3) // используются методы * и + для Double
    println(l4)
  }
}

object testHomework3 {
  @main def part3Ex2(): Unit ={
    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2
    println(l)
    println(l2)
    println(l3) // используются методы * и + для Logarithm
    println(l4)
  }
}