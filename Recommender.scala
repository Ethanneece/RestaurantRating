import scala.io.BufferedSource

case class RestaurantHistory
(user_id: String,
 rest_id: String,
 name: String,
 address: String,
 price: String,
 cuisine: String,
 rating: Int)

case class NearbyRestaurant
(rest_id: String,
 name: String,
 address: String,
 price: String,
 cuisine: String)

object Recommender {
  type RestID = String
  type UserID = String

  def readNearbyRestaurants(source: BufferedSource): List[NearbyRestaurant] = {
    val rows = getRows(source)
    rows.map {
      case Array(
      rest_id: String,
      name: String,
      address: String,
      price: String,
      cuisine: String) =>
        NearbyRestaurant(rest_id, name, address, price, cuisine)
      case _ => NearbyRestaurant("Invalid Record", "", "", "", "")
    }
  }

  def readUserRestaurantHistory(source: BufferedSource): List[RestaurantHistory] = {
    val rows = getRows(source)
    rows.map {
      case Array(
      user_id: String,
      rest_id: String,
      name: String,
      address: String,
      price: String,
      cuisine: String,
      rating: String) =>
        RestaurantHistory(user_id, rest_id, name, address, price, cuisine, rating.toInt)
      case _ => RestaurantHistory("Invalid Record", "", "", "", "", "", -1)
    }
  }

  def getRows(source: BufferedSource): List[Array[String]] = {
    val lines = source.getLines().toList
    //skip the header row
    lines.drop(1).map(l => l.split(",").map(_.trim))
  }


  /**
 	This function computes all required coefficients and scores
	@param userId -- the id of the user for whom this restaurant recommendation is being made
	@param nr2SimilarRest -- the mapping between the restaurant ids in the vicinity and "similar" 
	restaurants that have been ranked; notice that the notion of "similarity" is given by the SimilarityMarker
	parameter to the main function
	@param  resID2RatingVector -- the mapping between the restaurant ids (from the dining history dataset) to 
	a list of tuples;
	each tuple (userID, rating), the rating of this user for that restaurant; notice that all userID's must 
	be included--if a user has not ranked a restaurant, this user's ranking is 0.
	@return the mapping between the restarant ids in the vicinity and a list of triples 
	(restaurantID, S_ik from the ranking formula, w_jk from the ranking formula) 
  */ 
  def computeCoefficientsAndScores(userId: String, nr2SimilarRest: Map[String, List[String]], 
				    resID2RatingVector: Map[String, List[(String, Int)]]):
  				    Map[String, List[(String, Double, Double)]] = {
	return ComputeAll.computeCoefficientsAndScores(userId, nr2SimilarRest, resID2RatingVector) 
  }
  
   def main(args: Array[String]): Unit = {

    if (args.length != 4) {
      println("Usage scala Recommender DirWithCSVFiles NearbyRestaurants.csv UserID SimilarityMarker")
      System.exit(-1)
    }

    val allCSVLst = new java.io.File(args(0)).listFiles.filter(_.getName.endsWith(".csv"))

    val bufferedSources = allCSVLst.toList.map(fileName => io.Source.fromFile(fileName, "ISO-8859-1"))
    val llHistories:List[List[RestaurantHistory]] = bufferedSources.map(bs => readUserRestaurantHistory(bs))
    val nearbyRestaurants:List[NearbyRestaurant] = readNearbyRestaurants(io.Source.fromFile(args(1), "ISO-8859-1")).sortWith((r1, r2) => r1.name < r2.name)
    val userId = args(2)
    val simMarker = args(3)
    if (!simMarker.equals("c") && !simMarker.equals("p&c") && !simMarker.equals("p|c") && !simMarker.equals("p")) {
      println("The SimilarityMarker parameter must be one of: c, p&c, p|c, p")
      System.exit(-1)
    }


    // Get a list of userIds and restIds
    val userIds = llHistories.flatten.map(_.user_id).distinct
    val restIds = llHistories.flatten.map(_.rest_id).distinct

    // Different ways to sort the output by
    //Either by cuisine, price, price and cuisine, price or cuisine
    val similarity = simMarker match {
      case "c" => nearbyRestaurants.groupBy(x => x).map(x => (x._1.rest_id, 
        llHistories.flatten.filter(y => (y.rest_id != x._1.rest_id) && y.cuisine == x._1.cuisine).map(y => y.rest_id).distinct))
      case "p" => nearbyRestaurants.groupBy(x => x).map(x => (x._1.rest_id, 
        llHistories.flatten.filter(y => (y.rest_id != x._1.rest_id) && y.price == x._1.price).map(y => y.rest_id).distinct))
      case "p&c" => nearbyRestaurants.groupBy(x => x).map(x => (x._1.rest_id, 
        llHistories.flatten.filter(y => (y.rest_id != x._1.rest_id) && y.cuisine == x._1.cuisine && y.price == x._1.price).map(y => y.rest_id).distinct))
      case "p|c" => nearbyRestaurants.groupBy(x => x).map(x => (x._1.rest_id, 
        llHistories.flatten.filter(y => (y.rest_id != x._1.rest_id) && (y.cuisine == x._1.cuisine || y.price == x._1.price)).map(y => y.rest_id).distinct))
    } 

    //Vector of rest_id that holds user_id and their rating 
    val mapping = restIds.groupBy(x => x).map(x => (x._1, userIds.map(y => 
      (y, checkExist(llHistories.flatten.find(z => z.rest_id == x._1 && z.user_id == y))))))


    // Compute scores based on user ratings
    val computed = computeCoefficientsAndScores(userId, similarity, mapping)
    val ranking = nearbyRestaurants.map(x => (x, computed(x.rest_id).map(
      y => y._2 * y._3).reduceOption(_ + _).getOrElse(0.0) / 
      computed(x.rest_id).map(y => y._3).reduceOption(_ + _).getOrElse(0.0)))

    //Grab the resteraunt with the highest rating 
    val sortedRanking = ranking.filter(x => !x._2.isNaN).sortWith((x, z) => x._2 > z._2)

    // Find top ranking and sort by name if multiple resteraunts have same rating
    //Choose by name if rankings are same. 
    val topRanking = sortedRanking.head._2
    val sortedRankingByName = sortedRanking.filter(
        x => x._2 >= topRanking).sortWith((x, z) => x._1.name < z._1.name)
    
    println(sortedRankingByName.head._1.name + " located at " + sortedRankingByName.head._1.address)
    bufferedSources.foreach(_.close())
  }

  def checkExist(option: Option[RestaurantHistory]): Int = {
    option match {
      case Some(value) => value.rating
      case None => 0
    }
  }
}
