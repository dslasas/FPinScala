import Stream._

sealed trait Stream[+A] {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /* Write a function to convert a Stream to a List, which will force it's
     evaluation and let you look at it in the REPL. You can convert to the regular
     List type in the standard library. You can place this and other functions
     that operate on a stream inside the Stream trait.
  */
  /* Doesn't compile
  def toList: List[A] =  this match {
    case Empty => List()
    case Cons(h, t) => List(h()) :: t().toList
  }
  */

  /* Write the function take(n) for returning the first n elements of a stream,
     and drop(n) for skipping the first n elements of a Stream */
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => if (n == 0)
                      cons(h(), t().take(n-1))
                      else cons(h(), empty)
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => if (n > 0)
                      t().drop(n-1)
                      cons(h(), t()) /* or can use 'this' */
  }

  /* Exercise 5.3 Write the function takeWhile for returning all starting elements
     of a Stream that match the given predicate.
  */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => if (p(h()))
                      cons(h, t().takeWhile(p))
                      t().takeWhile(p)
  }

  /* This looks very similar to the foldRight we wrote for List, but note how
     our combinding function f is non-strict in its second parameter. If f
     chooses not to evaluate it's second parameter, this terminates the traversal
     early. We can see this by using foldRight to implement exists :

     def exists(p: A => Boolean): Boolean =
      foldRight(false)((a,b) => p(a) || b)

  */
  def foldRight[B](z: => B)(f: (A, => B) => B) : B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  /* Exercise 5.4 Implement forAll, which checks that all elements in the stream
     match a given predicate. Your implementation should terminate the traversal
     as soon as it encounters a nonmatching value.
  */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)


  /* Exercise 5.5 Use foldRight to implement takeWhile */
  def takeWhile_2(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) ((a,b) => if (p(a)) cons(a,b)
                                  else empty)

  /* Exercise 5.6 [Hard] Implement headOption using foldRight */

  /* Exercise 5.7 Implement map, filter, append, and flatMap using foldRight.
     The append method should be non-strict in it's argument. */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A]) ((a,b) => if (f(a)) cons(a,b)
                                   else b)

  def append[B >: A] (s: => Stream[B]) : Stream[B] =
    foldRight(s) ((a,b) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) ((a,b) => f(a) append b)

  /* Exercise 5.8 Generalize ones slightly to the function constant, which
     returns an infinite Stream of a given value.  */
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /* Exercise 5.9 Write a function that generates an infinite stream of integers,
     starting from n, then n+1, n+2 and so on.
  */
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  /* Exercise 5.10 Write a function fibs that generates the infinite stream of
     Fibonacci numbers 0,1,1,2,3,5,8 and so on
   */
   def fib(): Stream[Int] = {
      def loop(a: Int, b: Int): Stream[Int] = {
        cons(a, loop(a+b, b))
      }
      loop(0, 1)
   }

   /* Exercise 5.11 Write a more general stream-building function called unfold.
      It takes a function for producing both the next state and the next value
      in the generated stream.
   */
   def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
      case Some((h,t)) => cons(h, unfold(t)(f))
      case None => empty
   }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]































/* */
