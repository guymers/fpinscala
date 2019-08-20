import java.util.concurrent.ExecutorService
import java.util.concurrent.Future

package object chapter7 {

  type Par[A] = ExecutorService => Future[A]
}
