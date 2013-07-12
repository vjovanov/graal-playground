package playground

object test {
  def method() = 1
}

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

  def methodCalls(arg: Int) = {
    println("")
    100000
  }

  def arithmetics(arg: Int) = {
    val x = 1
    val y: Short = 1
    x + 1.0
    x + 1L
    x + y
  }
}