package playground

object test {
  def method() = 1
}

class Examples {
  def simpleIf(arg: Int) = {
    val cond = arg > 1
  	val res = if(cond) 1 else 2
    res
  }

  def loop(arg: Int) = {
    var i = 0
    while ({val c = i < arg; c}) {
      i = i + 1
    }
    arg
  }

  def methodCalls(arg: Int) = {
    println("")
    100000
  }

  def arrays(arg: Int) = {
      val a = new Array[Int](arg)
      a(1) = 2
      val x = a(0)
      val y = a.length
      x + y
  }

  def arithmetics(arg: Int) = {
    val x = 1
    val y: Short = 1
    x + 1.0
    x + 1L
    x + y
    x % 1
    val az = x == 1
    val c = 1 == true
    val z = 1 == 1L
    val res = "abc" == 1
    res
  }

  def nestedIfs(x: Int) = {
    if (x < 01) if(x < -11) -12 else 13 else if (x > 21) 22 else x - 32
  }

  def dummyWhile(x: Int) = {
    val x = while(true){()}
    x
  }
}