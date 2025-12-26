package fpinscala.exercises.datastructures

// cd ~/fpinscala; scala console .
// import fpinscala.exercises.datastructures.List.*

// Functional datastructures are immutable by definition since side effects,
// including modifying data in-place, are not allowed.
/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  // O(m) where m = a1.length
  // Every iter creates a new Cons node for each element of a1 in O(1) (pattern
  // match and create node)
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  // Call stack "folds right" once eval starts, right to left.
  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Remove first element of list l, return tail. O(1), because there's no 
  // copying, just referencing existing nodes.
  def tail[A](l: List[A]): List[A] = l match
      case Nil => sys.error("List l is empty.")
      case Cons(_, t) => t
    
  // Replace head. O(1)
  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
    
  // Drop first n elements from a list. O(n)
  def drop[A](l: List[A], n: Int): List[A] = 
    if n <= 0 then l
    else l match
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
  
  // Drop prefix elements while predicate true. O(n) n = l.length
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l

  // Return all but last element. O(n) because singly-linked list, need to 
  // copy every node except last while traversing to last node.
  def init[A](l: List[A]): List[A] = l match
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil  // if last node, return Nil to drop last node
    case Cons(h, t) => Cons(h, init(t))
  
  def length[A](l: List[A]): Int = 
    foldRight(l, 0, (_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)
  
  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)
    
  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A](), (acc, h) => Cons(h, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A], append)

  def incrementEach(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int], (h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil:List[String], (h,t) => Cons(h.toString,t))

  def map[A,B](l: List[A], f: A => B): List[B] = foldRight(l, Nil:List[B], (h, t) => Cons(f(h), t)) // not stack safe because of foldRight

  def filter[A](as: List[A], f: A => Boolean): List[A] = foldRight(as, Nil: List[A], (h, t) => if f(h) then Cons(h, t) else t) // not stack safe because of foldRight

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))

  def zipWith[A,B,C](a: List[A], b: List[B], f: (A,B) => C): List[C] = (a,b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
