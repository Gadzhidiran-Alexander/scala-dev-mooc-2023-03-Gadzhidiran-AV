package module1.futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличие от нее,
   * возвращающую все успешные и неуспешные результаты.
   * Возвращаемый тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правом результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    futures.foldRight(Future((List.empty[A], List.empty[Throwable]))) {
      (fa, fr) =>
          fa.flatMap(res =>
            fr.map(agg => (res :: agg._1, agg._2))
          ).recoverWith(e =>
            fr.map(agg => (agg._1, e :: agg._2)))
    }
  }
}
