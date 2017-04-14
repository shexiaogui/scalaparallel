package week1

/**
  * Created by shexiaogui on 14/04/17.
  */
class Account(private var amount: Int) {
  val uid = new UIDThread().getUniqueId()
  
  def transfer(target: Account, n: Int) = {
    if(this.uid < target.uid) lockAndTransfer(this, target, n)
    else lockAndTransfer(target, this, n)
  }
   
  
  def lockAndTransfer(source: Account, target: Account, n: Int) =
    source.synchronized{
      target.synchronized{
        target.amount += n
        source.amount -= n
      }
    }
  def startThread(a: Account, b: Account, n: Int): Thread ={
     val t = new Thread{
       override def run(): Unit = {
         for(i <- 0 until n) a.transfer(b, 1)
       }
     }
     t
   }
  
  
  def main(args: Array[String]): Unit = {
    this.amount = 90000
    val b = new Account(90000)
    val t = startThread(this, b, 80)
    val s = startThread(b, this, 80)
    t.join
    s.join
  }
}
