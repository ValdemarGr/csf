package training
import scala.collection.immutable.List
object Example{
  val message="héllo"
  val values:List[Int]=List(1,2,3)
  def choose(flag:Boolean,left:Int,right:Int):Int=if(flag)left else right
  def label(value:Int):String=value match{case 0=>"zero";case _=>"other"}
  val mapped=values.map((value:Int)=>choose(true,value,0))
}
