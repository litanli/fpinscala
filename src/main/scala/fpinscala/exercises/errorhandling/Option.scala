package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  // Use map when f returns a plain value, flatMap when f returns a wrapped value.

  // Apply f if the Option is not None. Transforms the value INSIDE a container 
  // (Some). 
  // E.g. Some(2).map(x => x * 2) returns Some(4)
  //      None.map(x => x * 2) returns None
  //      Some(2).map(x => Some(x * 2)) returns Some(Some(4)). If f already returns
  //      an Option container, you get a nested Option, which is almost never
  //      what you want. Use flatMap to flatten it out.
  def map[B](f: A => B): Option[B] = this match
    case Some(a) => Some(f(a))
    case None => None
  
  def getOrElse[B>:A](default: => B): B = this match
    case Some(a) => a  // flatten/unpack
    case None => default
  
  // Apply f, which may fail, to the Option if not None. Transforms the value
  // into another container, then flattens the result.
  // E.g. Some(2).flatMap(x => Some(x * 2)) returns Some(4)
  def flatMap[B](f: A => Option[B]): Option[B] = 
    // map(f) is of type Option[Option[B]]
    // For Some, it's Some(Some())
    map(f).getOrElse(None)

  // Don't eval ob unless needed
  def orElse[B>:A](ob: => Option[B]): Option[B] = 
    map(Some(_)).getOrElse(ob)

  // Convert Some to None if the value doesn't satify f
  def filter(f: A => Boolean): Option[A] = 
    flatMap(a => if (f(a)) Some(a) else None)

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // Combines two Option values using a finary function. if either Option value
  // is None, then return value is None.
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  // Combines list of Options into one Option containing list of all Some values
  // from original list. If any None, result of whole function is None
  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case Nil => Some(Nil)
    case h::t => h.flatMap(hh => sequence(t).map(hh :: _))
  
  // Map over a list using a function that might fail, return None if applying 
  // it to any element of the list returns None. Doing so with sequence(as.map(a => f(s)))
  // traverses the list twice, first to map then to sequence. Do so with one list
  // traversal.
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  
