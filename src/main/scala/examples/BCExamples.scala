package playground

object test {
  def method() = 1
}

class Examples {
  def boolean(arg: Int) = true

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
    // invoke on an object
    val x = "blimp" concat "blomp"

    // access on the static field
    val y = println("klomp")
    println(y)

    // field access on an object
    "blah".length

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

  def conditionals(arg: Int) = {
    val d = Double.NaN
    val f = arg.toFloat
    val l = arg.toLong
    val resi0 = arg < 1
    val resi1 = arg > 0
    val resl0 = l > 1
    val resl1 = l < 0
    val resl2 = l <= 1
    val resd0 = d < 1.0D
    val resd1 = d <= 0.0D
    val resd2 = d > 1.0D
    val resd3 = d >= 0.0D
    val resd4 = d == 1.0D
    val resd5 = d != 0.0D
    val resf0 = f < 1.0f
    val resf1 = f > 1.0f
    val resf2 = f >= 1.0f

    (resd0, resf0, resi0, resl0)._3
  }

  def nestedIfs(x: Int) = {
    if (x < 01) if(x < -11) -12 else 13 else if (x > 21) 22 else x - 32
  }

  def dummyWhile(x: Int) = {
    val x = while(true){()}
    x
  }

  def invocations(x: Int) = {
    val r = java.lang.Integer.toString(x)
    r

    val res = "a" concat "b"
    Predef.println(res)
    res

  }

  def booleanOps(x: Int) = {
    val x = true
    val y = false
    val r1 = x && y
    val r2 = x || y
    val r3 = !x
    r3
  }
}