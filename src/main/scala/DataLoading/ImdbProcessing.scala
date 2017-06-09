package thesis.dataLoading

import java.io._

import scala.collection.mutable.Set

object IMDBProcessing{

	def preProcess{
		val filename = "/Datasets/IMDB/movie_metadata_withDelimiters.csv"

		val iterator = LoadDataset.getFileFromResources(filename)

		val genreSet = Set[String]()

		val emptySet = Set[String]()

		val theIt = iterator.drop(1).map{ line =>
			val s = line.split("\\$")


			/* 
				Index where the values are
				6 --> actor 2 name
				9 --> genre
				10 --> actor 1 name
				14 --> actor 3 name
			*/

			val actor1 = s(10).toLowerCase
			val actor2 = s(6).toLowerCase
			val actor3 = s(14).toLowerCase
			val genre = s(9).split("\\|").toSet
			genre.foreach{ g => 
				genreSet += g
			}

			List(
				(actor1 -> genre),
				(actor2 -> genre),
				(actor3 -> genre)
			)
		}
		.toList // List of List(actor1 -> genre, actor2 -> genre, actor3 -> genre)
		.flatMap(x => x) // List of (actorA -> genre, actorA -> genre, actorB -> genre, ...)
		.groupBy(_._1) // Group by actor name: Map of [actorA -> List(actorA -> Set(genre1), actorA -> Set(genre2)), actorB -> List(...)]
		.mapValues{
			_.foldLeft(emptySet){case(acc, tuple) =>  // Aggregate the list of (actor -> Set)
				acc.union(tuple._2) // Add to the accumulator for this actor each Set of genre
			}
		}
		
		println(genreSet)

		val genreSetIndex = genreSet.zipWithIndex.toMap

		val file = new File("movie_processed.txt")
		val bw = new BufferedWriter(new FileWriter(file))

		bw.write(genreSetIndex.mkString(",")+ "\n")

		theIt.mapValues{_.map{genre => genreSetIndex(genre)}}
		.foreach{ case(actor,genreIndexList) =>
			bw.write(s"$actor,${genreIndexList.mkString(",")}\n")
		}

		bw.close()
	}
}