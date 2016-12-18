def factorial(n: Int)={
  def factorialIter(n : Int, fact : Int) : Int =
    if(n-1 == 0) fact else factorialIter(n-1, fact*n)

  factorialIter(n, 1);
}

factorial(5)