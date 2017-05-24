
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
