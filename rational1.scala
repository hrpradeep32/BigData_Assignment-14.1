
// to perform sum

object rational1 {
  
var i = 1; 
var x = new Rational(0, 1); 
while (i <= 10) { x = x + new Rational(1,i); i = i + 1; } 
System.out.println("" + x.numer + "/" + x.denom); 
}