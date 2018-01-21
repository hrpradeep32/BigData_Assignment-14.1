

object rational3 {
  class Rational(n: int, d: int) 
  { 
    private def gcd(x: int, y: int): int = { 
      if (x == 0) y else if (x < 0) gcd(-x, y) 
      else if (y < 0) -gcd(x, -y) else gcd(y % x, x); 
      } 
    private val g = gcd(n, d); 
    val numer: int = n/g; 
    val denom: int = d/g; 
    def +(that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom); 
    def -(that: Rational) = new Rational(numer * that.denom - that.numer * denom, denom * that.denom); 
    def *(that: Rational) = new Rational(numer * that.numer, denom * that.denom); 
    def /(that: Rational) = new Rational(numer * that.denom, denom * that.numer); } 
}