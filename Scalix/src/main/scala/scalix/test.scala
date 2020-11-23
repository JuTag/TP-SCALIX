package scalix

import java.io.PrintWriter

import org.json4s.JsonAST.{JArray, JInt, JString, JValue}
import org.json4s.native.JsonParser.parse

import scala.io.Source


object Test extends App {

  //print(findActorId("David","Lynch"))
  //print (findActorMovies(287))
  //print(findMovieDirector(550))
  val actor1 = new FullName("Christian", "Bale")
  val actor2 = new FullName("Michael", "Caine")

  print(request(actor1,actor2))


  def findActorId(name: String, surname: String):Option[Int] = {
    val url = "https://api.themoviedb.org/3/search/person?api_key=4f6ee73c5f67065f7217e9048486969e&language=fr-FR&include_adult=false&page=1&query="
    val response = Source.fromURL(url + name + "-" + surname)
    val responseJson = parse(response.mkString)
    val JInt(totalResults) = responseJson.\("total_results")
    if (totalResults != 0) {
      val results = responseJson.\("results").\("id")
      val JInt(res) = results match {
        case JInt(x) => x
        case JArray(arr) => arr(0)
      }
      Some(res.toInt)
    } else {None}

  }

  def findActorMovies(id: Int): Set[(Int,String)] = {
    var resultSet: Set[(Int,String)] = Set()
    val url1 = "https://api.themoviedb.org/3/person/"
    val url2= "/movie_credits?api_key=4f6ee73c5f67065f7217e9048486969e&language=en-US&include_adult=false&page=1"
    val response = Source.fromURL(url1 +id+ url2)
    val responseJson = parse(response.mkString)
    val JArray(results) = responseJson.\("cast")
    if (results.nonEmpty) for (res <- results) {
      val JInt(i)= res.\("id")
      val JString(s)= res.\("title")
       resultSet = resultSet ++ Set((i.toInt ,s))
    }
    val out = new PrintWriter(String.format("..\\Scalix\\src\\main\\scala\\data\\actor%d.txt",id))
    out.write(resultSet.toString())//TODO: retourne des fichiers vides
    resultSet
  }

  def findMovieDirector(id: Int): Option[(Int, String)] = {
    val url1 = "https://api.themoviedb.org/3/movie/"
    val url2= "/credits?api_key=4f6ee73c5f67065f7217e9048486969e&language=en-US&include_adult=false&page=1"
    val response = Source.fromURL(url1 +id+ url2)
    val responseJson = parse(response.mkString)
    val JArray(results) = responseJson.\("crew")
    var dir:Option[(Int, String)] =None
    if (results.nonEmpty) for (res <- results) {
      val JString(s)= res.\("job")
      if (s == "Director") {
        val JInt(i)= res.\("id")
        val JString(name)= res.\("name")
        dir = Some((i.toInt,name))
      }
    }
    dir
  }

  def request(actor1: FullName, actor2: FullName): Set[(String, String)]= {
    val movieList1 = findActorMovies(findActorId(actor1.name,actor1.surname).get)
    val movieList2 = findActorMovies(findActorId(actor2.name,actor2.surname).get)
    val commonMovies = for (x<-movieList1;y<-movieList2 if x==y ) yield x
    for (x<-commonMovies) yield (x._2,findMovieDirector(x._1).get._2)
  }


}


class FullName(val name:String, val surname:String){
}

