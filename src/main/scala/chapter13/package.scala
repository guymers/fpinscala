import chapter7.nonblocking.Par

package object chapter13 {

  type IO[A] = Free[Par, A]
}
