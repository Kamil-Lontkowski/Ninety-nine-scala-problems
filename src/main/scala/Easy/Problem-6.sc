// Check if is palindrome
def isPalindrome[A](xs: List[A]): Boolean =
  xs == xs.reverse

isPalindrome(List(1, 2, 3, 2, 1))

//Polymorfic function value
val isPal = [A] => (xs: List[A]) =>
  xs == xs.reverse

isPal(List(1, 2, 3, 2, 1))