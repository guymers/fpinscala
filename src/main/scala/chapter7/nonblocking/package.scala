package chapter7

import java.util.concurrent.ExecutorService

package object nonblocking {

  type Par[+A] = ExecutorService => Future[A]
}
