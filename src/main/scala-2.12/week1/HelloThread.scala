package week1

/**
  * Created by shexiaogui on 14/04/17.
  */
class HelloThread extends Thread{
  override def run(): Unit = println("Hello"); println("World")
}

object main {
  val t = new HelloThread
  val s = new HelloThread
  t.start()
  s.start()
  t.join()
  s.join()
  
  
  
  
}


