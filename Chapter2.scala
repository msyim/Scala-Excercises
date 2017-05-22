/** Exercise 1 : Write a function to get the n-th Fibonacci number. The
first two Fibonacci numbers are 0 and 1, and the next number is always the sum of
the previous two. Your definition should use a local tail-recursive function.
**/

// Non-tail recursive version
def fib(n: Int): Int = {
  if ( n <= 1 ) 1
  else fib(n-1) + fib(n-2)
}

// Tail-recursive version
def fib(n: Int): Int = {
  @annotation.tailrec
  def go(before: Int, current: Int, index: Int): Int = 
    if(index == n-1) before+current
    else go(current, before+current, index+1)
  go(0,1,0)
}

/** Exercise2 : Implement isSorted, which checks whether an Array[A] is
sorted according to a given comparison function.**/
def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(current: Int, end: Int): Boolean = {
    if (current == end) True
    else { 
      if (gt(current,current+1)) False
      else go(current+1,end)
    }
  }
  go(0,as.length-1)
}

/** Exercise3 (hard): Implement partial1 and write down a concrete usage
of it. There is only one possible implementation that compiles. **/
def partial1[A,B,C](a:A, f:(A,B) => C): B => C = {
  def helper(a:A)(b:B): C = f(a,b)
  helper(a)
}

/** Exercise4 (hard): Let's look at another example, currying, which converts a
function of N arguments into a function of one argument that returns another
function as its result **/
def curry[A,B,C](f:(A,B)=>C): A=>(B=>C) = {
  def helper1(a:A)(b:B):C = f(a,b)
  def helper2(a:A): B=>C = helper1(a)
  helper2
}

/** Exercise5 (optional): Implement uncurry, which reverses the
transformation of curry. Note that since => associates to the right, A => (B
=> C) can be written as A => B => C. **/
def uncurry[A,B,C](f: A=>B=>C):(A,B)=>C = {
 def helper(a:A, b:B):C = f(a)(b)
 helper
}

/** Exercise6 : Implement the higher-order function that composes two
functions. **/
def compose[A,B,C](f: B => C, g: A => B): A => C = {
 def helper(a:A): C = f(g(a))
 helper
}

