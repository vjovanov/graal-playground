package playground

class Examples {
  def simpleIf(arg: Int) = {
  	if(arg > 1) 1 else 2
  }

  def loop(arg: Int) = {
  	var sum = 0
    var i = 0
    while (i < arg) {
      sum += i
      i += 1
    }
    sum
  }
}