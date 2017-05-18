// Exercise 1 : Write a function to get the n-th Fibonacci number. The
// first two Fibonacci numbers are 0 and 1, and the next number is always the sum of
// the previous two. Your definition should use a local tail-recursive function.

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
