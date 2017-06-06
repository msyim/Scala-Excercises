
/** Exercise1: What will the result of the following match expression be?

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}

**/
// matches the third case, so 1 + 2 =3

/** Exercise2: Implement the function tail for "removing" the first element
of a List. Notice the function takes constant time. What are different choices you
could make in your implementation if the List is Nil? We will return to this
question in the next chapter. **/
def tail[A](l:List[A]): List[A] = l match{
  case Nil => Nil
  case Cons(x,xs) => xs
}

/** Exercise3: Generalize tail to the function drop, which removes the first
n elements from a list. **/
def drop[A](l: List[A], n: Int): List[A] = l match{
  case Nil => Nil
  case Cons(x,xs) => drop(xs, n-1)
}

/** Exercise4: Implement dropWhile,10 which removes elements from the
List prefix as long as they match a predicate. Again, notice these functions take
time proportional only to the number of elements being dropped—we do not need
to make a copy of the entire List. **/
def dropWhile[A](l:List[A])(f: A=>Boolean): List[A] = l match{
  case Nil => Nil
  case Cons(x,xs) => if(f(x)) dropWhile(xs)(f) else Cons(x,dropWhile(xs)(f))
}

/** EXERCISE 5: Using the same idea, implement the function setHead for
replacing the first element of a List with a different value. **/
def setHead[A](l:List[A], new_head:A): List[A] = l match{
  case Nil => Cons(new_head,Nil)
  case Cons(x,xs) => Cons(new_head,xs)
}

/** EXERCISE 6: Not everything works out so nicely. Implement a function,
init, which returns a List consisting of all but the last element of a List. So,
given List(1,2,3,4), init will return List(1,2,3). Why can't this
function be implemented in constant time like tail?**/
def init[A](l:List[A]):List[A] = l match{
  case Nil => Nil
  case Cons(x,Nil) => Nil
  case Cons(x,xs) => Cons(x,init(xs))
}

/** EXERCISE 7: Can product implemented using foldRight immediately
halt the recursion and return 0.0 if it encounters a 0.0? Why or why not? **/
def product(l:List[Double]): Double = {
  foldRight(l,1.0)(_*_)
}
// It cannot halt when 0.0 is met because foldRight is not tail-recursive.

/** EXERCISE 8: See what happens when you pass Nil and Cons themselves to
foldRight, like this: foldRight(List(1,2,3),Nil:List[Int])(Cons(_,_)) **/

// foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
// = Cons(1, foldRight(List(2,3), Nil:List[Int]) )
// = Cons(1, Cons(2, foldRight( List(3), Nil:List[Int] ) ) )
// = Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int]) ) ) )
// = Cons(1, Cons(2, Cons(3, Nil) ) ) = List(1,2,3)

/** EXERCISE 9: Compute the length of a list using foldRight. **/
def length[A](l: List[A]): Int = {
  foldRight(l,0)(1+_)
}
