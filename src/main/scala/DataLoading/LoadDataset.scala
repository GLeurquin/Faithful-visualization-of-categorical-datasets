package thesis.dataLoading

import thesis.matrixTypes._
import thesis.utils._
import thesis.rectangles._

import scala.io.Source
import java.io.InputStream

import breeze.linalg._

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import scala.collection.mutable.BitSet
import scala.collection.mutable.ArrayBuffer

import breeze.linalg._
import breeze.plot._
import scala.collection.mutable.ArrayBuffer

import java.awt.Paint
import java.awt.Color

object LoadDataset extends MatrixHelpers{

	val SEED = 545173132
	// val mammalsX = Array("Alces_alces","Alopex_lagopus","Apodemus_agrarius","Apodemus_alpicola","Apodemus_flavicollis","Apodemus_mystacinus","Apodemus_sylvaticus","Apodemus_uralensis","Arvicola_sapidus","Arvicola_terrestris","Atelerix_algirus","Canis_aureus","Canis_lupus","Capra_ibex","Capra_pyrenaica","Capreolus_capreolus","Castor_fiber","Cervus_elaphus","Chionomys_nivalis","Clethrionomys_glareolus","Clethrionomys_rufocanus","Clethrionomys_rutilus","Cricetus_cricetus","Crocidura_leucodon","Crocidura_russula","Crocidura_suaveolens","Dama_dama","Dinaromys_bogdanovi","Dryomys_nitedula","Eliomys_quercinus","Erinaceus_concolor","Erinaceus_europaeus","Felis_silvestris","Galemys_pyrenaicus","Genetta_genetta","Glis_glis","Gulo_gulo","Hystrix_cristata","Lemmus_lemmus","Lepus_corsicanus","Lepus_europaeus","Lepus_granatensis","Lepus_timidus","Lutra_lutra","Lynx_lynx","Lynx_pardinus","Marmota_marmota","Martes_foina","Martes_martes","Meles_meles","Mesocricetus_newtoni","Micromys_minutus","Microtus_agrestis","Microtus_arvalis","Microtus_cabrerae","Microtus_duodecimcostatus","Microtus_gerbei","Microtus_guentheri","Microtus_lusitanicus","Microtus_multiplex","Microtus_oeconomus","Microtus_rossiaemeridionalis","Microtus_savii","Microtus_subterraneus","Microtus_thomasi","Mustela_erminea","Mustela_eversmanii","Mustela_lutreola","Mustela_nivalis","Mustela_putorius","Myopus_schisticolor","Nannospalax_leucodon","Neomys_anomalus","Neomys_fodiens","Nyctereutes_procyonoides","Ovis_ammon","Pteromys_volans","Rangifer_tarandus","Rupicapra_pyrenaica","Rupicapra_rupicapra","Sciurus_vulgaris","Sicista_betulina","Sorex_alpinus","Sorex_araneus","Sorex_caecutiens","Sorex_coronatus","Sorex_granarius","Sorex_isodon","Sorex_minutissimus","Sorex_minutus","Sorex_samniticus","Spermophilus_citellus","Suncus_etruscus","Sus_scrofa","Talpa_caeca","Talpa_europaea","Talpa_occidentalis","Talpa_romana","Ursus_arctos","Vormela_peregusna","Vulpes_vulpes")

	/**
	*	Reads the file from the resources
	*	@param Filename the path in resources to the file to be read
	*	@return an iterator on the lines of that file
	*/
	def getFileFromResources(filename:String):Iterator[String] = {
		val stream = getClass.getResourceAsStream(filename)
		Source.fromInputStream(stream).getLines
	}

	/**
	*	Removes the columns with zero variance from matrix m. Does not modify m.
	*	Requires that m was not changed before (no moves were applied)
	*	@param m The matrix for which zero variance columns have to be removed
	*	@param symmetric If the matrix is square, should the rows be removed too ?
	*	@return A new matrix where the zero variance columns are removed (and the matching rows too if symmetric)
	*/
	private def removeZeroVariance(m:MatrixMoves, symmetric:Boolean=false):MatrixMoves = {
		if(symmetric) assert(m.cols == m.rows)
		val mat = m.getMatrix
		// Remove the 0 variance columns
		val keep = (0 until m.cols).filter{ c =>
			(0 until m.rows).map{r => m(r,c)}.distinct.size > 1
		}.toSet

		val oldIdx2NewIdx = keep.toList.sorted.zipWithIndex.toMap

		val noZeroVar = m.selectAll{_ =>
			val b = new BitSet(m.rows * keep.size)
			keep.foreach{c =>
				(if(symmetric) keep else (0 until m.rows)).foreach{r =>
					if(m.getRawValue(r, c) == 1.0){
						if(symmetric) b += Utils.getHash(oldIdx2NewIdx(r), oldIdx2NewIdx(c), keep.size)
						else b += Utils.getHash(r, oldIdx2NewIdx(c), keep.size)
					}
				}
			}
			b
		}

		val labelX = m.labelX.zipWithIndex.withFilter{case (label, c) => keep.contains(c)}.map(_._1)

		val newRows = if(symmetric) keep.size else m.rows
		val labelY = if(symmetric) m.labelY.zipWithIndex.withFilter{case (label, c) => keep.contains(c)}.map(_._1) else m.labelY

		new BinarySparseMatrix(newRows, keep.size, noZeroVar, labelX=labelX, labelY=labelY, convolutionSettings=m.getConvolutionSettings,desc=m.desc)
	}

	/*********************
	*	  REAL DATASETS
	**********************/

	/**
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix representing the imdb dataset
	*/
	private def loadIMDB(convolutionSettings:ConvolutionSettings):MatrixMoves = {
		val imdbX = Array("War","Family","Western","Biography","Reality-TV","Thriller","News","Film-Noir","Music","Mystery","Adventure","Sci-Fi","Action","Animation","Sport","Short","Documentary","Crime","Game-Show","Comedy","Drama","Fantasy","History","Musical","Horror","Romance")
		val imdbY = Array.ofDim[String](6257)

		val filename = "/Datasets/IMDB/movie_processed.txt"

		val rows = 6257
		val cols = 26

		val imdb = new BitSet(rows*cols)

		val iterator = getFileFromResources(filename)

		var i = 0
		iterator.drop(1).foreach{ line =>
			val temp = line.split(",")

			imdbY(i) = temp(0)

			val splitted = temp.drop(1).map(_.toInt)
			splitted.foreach{ index =>
				imdb += Utils.getHash(i, index, cols)
			}
			i += 1
		}

		new BinarySparseMatrix(rows, cols, Array(imdb), convolutionSettings=convolutionSettings,labelX=imdbX, labelY=imdbY, desc="imdb")
	}

	/**
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix representing the mammals dataset
	*/
	private def loadMammals(convolutionSettings:ConvolutionSettings): MatrixMoves = {
		val mammalsX = Array("Acomys_minous","Alces_alces","Alopex_lagopus","Apodemus_agrarius","Apodemus_alpicola","Apodemus_flavicollis","Apodemus_mystacinus","Apodemus_sylvaticus","Apodemus_uralensis","Arvicola_sapidus","Arvicola_terrestris","Atelerix_algirus","Atlantoxerus_getulus","Bison_bonasus","Callosciurus_erythraeus","Callosciurus_finlaysonii","Canis_aureus","Canis_lupus","Capra_aegagrus","Capra_ibex","Capra_pyrenaica","Capreolus_capreolus","Castor_fiber","Cervus_elaphus","Chionomys_nivalis","Clethrionomys_glareolus","Clethrionomys_rufocanus","Clethrionomys_rutilus","Cricetulus_migratorius","Cricetus_cricetus","Crocidura_canariensis","Crocidura_leucodon","Crocidura_osorio","Crocidura_russula","Crocidura_sicula","Crocidura_suaveolens","Crocidura_zimmermanni","Dama_dama","Dinaromys_bogdanovi","Dryomys_nitedula","Eliomys_quercinus","Erinaceus_concolor","Erinaceus_europaeus","Felis_silvestris","Galemys_pyrenaicus","Genetta_genetta","Glis_glis","Gulo_gulo","Hystrix_cristata","Lemmus_lemmus","Lepus_capensis","Lepus_castroviejoi","Lepus_corsicanus","Lepus_europaeus","Lepus_granatensis","Lepus_timidus","Lutra_lutra","Lynx_lynx","Lynx_pardinus","Marmota_marmota","Martes_foina","Martes_martes","Meles_meles","Mesocricetus_newtoni","Micromys_minutus","Microtus_agrestis","Microtus_arvalis","Microtus_bavaricus","Microtus_cabrerae","Microtus_duodecimcostatus","Microtus_felteni","Microtus_gerbei","Microtus_guentheri","Microtus_lusitanicus","Microtus_multiplex","Microtus_oeconomus","Microtus_rossiaemeridionalis","Microtus_savii","Microtus_subterraneus","Microtus_tatricus","Microtus_thomasi","Mustela_erminea","Mustela_eversmanii","Mustela_lutreola","Mustela_nivalis","Mustela_putorius","Myomimus_roachi","Myopus_schisticolor","Nannospalax_leucodon","Neomys_anomalus","Neomys_fodiens","Nyctereutes_procyonoides","Ovis_ammon","Pteromys_volans","Rangifer_tarandus","Rupicapra_pyrenaica","Rupicapra_rupicapra","Sciurus_anomalus","Sciurus_vulgaris","Sicista_betulina","Sicista_subtilis","Sorex_alpinus","Sorex_araneus","Sorex_caecutiens","Sorex_coronatus","Sorex_granarius","Sorex_isodon","Sorex_minutissimus","Sorex_minutus","Sorex_samniticus","Spalax_graecus","Spermophilus_citellus","Spermophilus_suslicus","Suncus_etruscus","Sus_scrofa","Talpa_caeca","Talpa_europaea","Talpa_occidentalis","Talpa_romana","Talpa_stankovici","Tamias_sibiricus","Ursus_arctos","Vormela_peregusna","Vulpes_vulpes")
		val filename = "/Datasets/Mammals/mammals.txt"
		val rows = 2221
		val cols = 124

		/*
			Columns 0-68(inclusive) not binary
		*/
		val mammals = new BitSet(rows*cols)

		val iterator = getFileFromResources(filename)
		var i = 0
		iterator.foreach{ line =>
			val splitted = line.split(",").drop(69).map(_.toInt).zipWithIndex
			splitted.foreach{case(v, idx) =>
				if(v==1){
					mammals += Utils.getHash(i, idx, cols)
				}
			}
			i += 1
		}

		new BinarySparseMatrix(rows, cols, Array(mammals), labelX=mammalsX, convolutionSettings=convolutionSettings, desc="mammals")
	}

	/**
	*	@return An array with the column labels of the mushroom dataset
	*/
	private def getMushroomXlabel: Array[String] = {
		val filename = "/Datasets/Mushrooms/agaricus-lepiota.data.txt"
		val lines = getFileFromResources(filename)
		val filenameExpanded = "/Datasets/Mushrooms/mushrooms.txt"
		val linesExpanded = getFileFromResources(filenameExpanded)

		val theMap = new HashMap[(Int, Int), String]() // (Idx in non-expanded, Idx in expanded), Char corresponding to category

		for(line <- lines){
			val lineExpanded = linesExpanded.next()

			val splittedExpanded = lineExpanded.split(" ").drop(1).map(_.split(":").head.toInt)
			val splittedRaw = line.split(",")
			val splitted = (splittedRaw.take(11) ++ splittedRaw.drop(12)).drop(1) // remove 12th element


			splittedExpanded.zip(splitted).zipWithIndex.foreach{case((a,b), idx) =>
				val key = (idx+1,a)
				if(theMap.contains(key)) assume(theMap(key) == b, s"Iconsistent mapping for $a: have ${theMap(key)} and $b")
				else theMap += (key -> b)
			}
		}

		val initToCat = getFileFromResources("/Datasets/Mushrooms/initialToCategory.txt")

		val array = new Array[String](theMap.size+1)
		array(0) = "isPoisonous"
		// 1. cap-shape:bell=b,conical=c,convex=x,flat=f,knobbed=k,sunken=s
		var counter = 1
		for(line <- initToCat){
			val splitted = line.split(":")
			val category = splitted.head.split(" ").last
			val subcategories = splitted.last.split(",").map(_.split("=")).map{case Array(subcategory, initial) => (initial -> subcategory)}.toMap
			theMap.filter{case((reducedIdx, fullIdx),initial) => reducedIdx == counter}.foreach{case ((reducedIdx, fullIdx),initial) =>
				array(fullIdx) = s"$category:${subcategories(initial)}"
			}

			counter += 1
		}

		array
	}

	/**
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix representing the binary mushroom dataset
	*/
	private def loadMushrooms(convolutionSettings:ConvolutionSettings): MatrixMoves = {
		val filename = "/Datasets/Mushrooms/mushrooms.txt"
		val rows = 8124
		val cols = 112

		val mushrooms = new BitSet(rows*cols)

		val iterator = getFileFromResources(filename)

		val labelY = new Array[String](rows)

		var i = 0
		iterator.foreach{ line =>
			val splitted = line.split(" ")
			val poisonous = if(splitted.head.toInt == 1) "p" else "e" // 1 is poisonous, 2 is edible
			labelY(i) = poisonous

			splitted.drop(1).map(_.split(":").head.toInt).foreach{ x =>
				mushrooms += Utils.getHash(i, x-1, cols)
			}

			i += 1
		}

		val mush = new BinarySparseMatrix(rows, cols, Array(mushrooms), labelX=getMushroomXlabel, labelY=labelY, convolutionSettings=convolutionSettings,desc="mushrooms")
		removeZeroVariance(mush)
	}

	/**
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix representing the categorical mushroom dataset
	*/
	private def loadCategoricalMushrooms(convolutionSettings:ConvolutionSettings): MatrixMoves = {
		val mapping = Array(
			// "pe",
			"bcxfks",
			"fgys",
			"nbcgrpuewy",
			"tf",
			"alcyfmnps",
			"adfn",
			"cwd",
			"bn",
			"knbhgropuewy",
			"et", // 10
			"fyks", // 12
			"fyks",
			"nbcgopewy",
			"nbcgopewy",
			"pu",
			"nowy",
			"not",
			"ceflnpsz",
			"knbhrouwy",
			"acnsvy",
			"glmpuwd"
		)

		val labels = Array("cap-shape", "cap-surface", "cap-color", "bruises", "odor", "gill-attachment", "gill-spacing", "gill-size", "gill-color", "stalk-shape", "stalk-surface-above-ring", "stalk-surface-below-ring", "stalk-color-above-ring", "stalk-color-below-ring", "veil-type", "veil-color", "ring-number", "ring-type", "spore-print-color", "population", "habitat")

		val filename = "/Datasets/Mushrooms/agaricus-lepiota.data.txt"
		val rows = 8124
		val cols = 21
		val mushrooms = DenseMatrix.zeros[Double](rows, cols) // remove column at index 11

		val iterator = getFileFromResources(filename)

		val labelY = new Array[String](rows)

		var count = 0
		var i = 0
		iterator.foreach{ line =>
			var splitted = line.split(",")
			splitted = splitted.take(11) ++ splitted.takeRight(11)

			labelY(i) = splitted(0) // poisonous or edible
			splitted = splitted.drop(1) // Drop first elem

			val theMapping = splitted.zipWithIndex.map{case(a,i) =>
				val v = mapping(i).indexOf(a).toDouble
				assert(v >= 0.0, s"illegal mapping for $a at column $i")
				v
			}

			mushrooms(i, ::) := DenseVector(theMapping).t

			i += 1
		}

		val mush = BinarySparseMatrix(mushrooms,desc="mushroomsCAT", labelX=labels, labelY=labelY, convolutionSettings=convolutionSettings)
		removeZeroVariance(mush)
	}

	/**	Loads datasets coming from this : http://www.philippe-fournier-viger.com/spmf/index.php?link=datasets.php
	*
	*	@param filename The path of the sparse dataset
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix representing the imdb dataset
	*/
	private def loadSpaceSparse(filename:String, convolutionSettings:ConvolutionSettings): MatrixMoves = {

		var iterator = getFileFromResources(filename)

		val desc = filename.takeRight(filename.length-filename.lastIndexOf("/")-1).takeWhile(_ != '.')

		var rows = 0
		var cols = 0
		iterator.foreach{line =>
			cols = max(line.split(" ").map(_.toInt).max, cols)
			rows += 1
		}

		iterator = getFileFromResources(filename) // reset

		val data = DenseMatrix.zeros[Double](rows, cols)

		var mySet = Set[Int]()

		var i =0
		iterator.foreach{line =>
			line.split(" ").map(_.toInt).foreach{col =>
				data(i, col-1) = 1.0 // Cols start at 1
				mySet = mySet + (col-1)
			}
			i += 1
		}

		println((0 until cols).toSet -- mySet)

		BinarySparseMatrix(data,desc=desc, convolutionSettings=convolutionSettings)
	}

	/**
	*	@param name The name of the news dataset
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix representing the news dataset
	*/
	private def loadDocumentGraph(name:String, convolutionSettings:ConvolutionSettings): MatrixMoves = {
		val iterator = getFileFromResources("/Datasets/DocumentGraphs/"+name)
		(1 to 3).foreach{i => iterator.next()} // Skip first 3 lines


		val rows = iterator.next().split(" ").last.toInt
		val cols = iterator.next().split(" ").last.toInt

		var docData = DenseMatrix.zeros[Double](rows, cols)

		(0 until rows).foreach{ r =>
			docData(r,::) := DenseVector(iterator.next().split(" ").tail.map(_.toDouble)).t
		}

		docData = docData * docData.t

		(1 to 6).foreach(i => iterator.next()) // Skip 6 lines
		val labelY = Array.tabulate[String](rows){i =>
			iterator.next()
		}

		var max = 0.0
		for(r <- 0 until docData.rows; c <- 0 until docData.cols){
			if(docData(r,c) > max) max = docData(r,c)
		}

		docData = docData :/ max

		docData = docData.map{v =>
			if(v < 0.05) 0.0
			else if(v<0.1) 1.0
			else if(v<0.25) 2.0
			else 3.0
		}

		// val bitsets = docData.toOneHotEncoding
		// val m = new BinarySparseMatrix(docData.rows, docData.cols, Array(bitsets.last), convolutionSettings, null, labelY, "docGraph_"+name)

		val m = BinarySparseMatrix(docData, labelX=labelY, labelY=labelY, desc="docGraph_"+name, convolutionSettings=convolutionSettings)
		m.dontChangeColors = true
		removeZeroVariance(m, symmetric=true)
	}

	/**
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix representing the genes dataset
	*/
	private def loadGenes(convolutionSettings:ConvolutionSettings): MatrixMoves = {
		val rows = 2112
		val cols = 363

		val data = new BitSet(rows*cols)

		val iterator = getFileFromResources("/Datasets/Genes/tkde_binarized_data.txt")

		val labelX = iterator.next().split('\t')
		assert(labelX.length == cols, s"${labelX.length} != $cols")
		val labelY = new Array[String](rows)

		var row = 0
		iterator.foreach{ line =>
			val splittedLine = line.split('\t')
			labelY(row) = splittedLine.head

			var col = 0
			splittedLine.tail.map(_.toInt).foreach{ v =>
				if(v == 1){
					data += Utils.getHash(row,col, cols)
				}
				col += 1
			}
			assert(col == cols, s"$col != $cols")

			row += 1
		}

		assert(row == rows, s"$row != $rows")

		new BinarySparseMatrix(rows, cols, Array(data), convolutionSettings=convolutionSettings,labelX=labelX, labelY=labelY, desc="genes")
	}

	/**
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix representing the NOW dataset
	*/
	private def loadNOW(convolutionSettings:ConvolutionSettings): MatrixMoves = {
		val iterator = getFileFromResources("/Datasets/NOW/NOW_public_030717.csv")

		case class Paleo(lidnum:Long, location:String, country:String, sidnum:Long, species:String)
		case class Location(location:String, country:String){
			override def toString = s"$location-$country"
		}

		val locations = new HashMap[Long, Location] // Lidnum to location + country
		val species = new HashMap[Long, String] // sidnum to species


		iterator.next() // Skip first line
		val paleoData = iterator.map{line => 
			val splitted = line.split('|')

			val p = Paleo(splitted(0).toLong, splitted(1), splitted(13), splitted(36).toLong, splitted(21))

			val Location(location, country) = locations.getOrElseUpdate(p.lidnum, Location(p.location, p.country))
			// assert(p.location == location)
			// assert(p.country == country)

			val spec = species.getOrElseUpdate(p.sidnum, p.species)
			// assert(spec == p.species, s"$spec is not equal to ${p.species} for sidnum ${p.sidnum}")
			
			p
		}.toArray

		val lidnum2Idx = locations.keys.zipWithIndex.toMap
		val sidnum2Idx = species.keys.zipWithIndex.toMap

		// Locations on rows, species on columns
		val labelX = species.keys.toArray.sortBy{sidnum => sidnum2Idx(sidnum)}.map{sidnum => s"$sidnum-${species(sidnum)}"}
		val labelY = locations.keys.toArray.sortBy{lidnum => lidnum2Idx(lidnum)}.map{lidnum => s"$lidnum-${locations(lidnum)}"}

		val rows = lidnum2Idx.size
		val cols = sidnum2Idx.size
		val paleoBitset = new BitSet(rows * cols)

		paleoData.groupBy(_.lidnum).mapValues(_.map(_.sidnum)).foreach{case(lidnum, listOfSidnum) => 
			val row = lidnum2Idx(lidnum)
			listOfSidnum.foreach{ sidnum =>
				paleoBitset += Utils.getHash(row,sidnum2Idx(sidnum), cols)
			}
		}

		new BinarySparseMatrix(rows, cols, Array(paleoBitset), convolutionSettings=convolutionSettings,labelX=labelX, labelY=labelY, desc="paleo")
	}

	/**
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix representing the football dataset
	*/
	private def loadFootball(convolutionSettings:ConvolutionSettings) = {
		val iterator = getFileFromResources("/Datasets/Football/football.paj")
		val iterator_labels = getFileFromResources("/Datasets/Football/labels.txt")

		val rows = 115
		val cols = rows

		val labelX = Array.fill(rows)("")

		var count = 0

		iterator_labels.foreach{ line =>
			labelX(count) = line
			count += 1
		}

		var data = DenseMatrix.zeros[Double](rows, cols)

		while(iterator.next() != "*Edges"){}

		iterator.foreach{line =>
			val Array(i, j) = line.split(" ").map(_.toInt-1)
			data(i, j) = 1.0
			data(i, i) = 1.0
			data(j, i) = 1.0
		}

		BinarySparseMatrix(data, desc="football", labelX = labelX, labelY = labelX, convolutionSettings=convolutionSettings)
	}

	/************************
	*	ARTIFICIAL DATASETS
	*************************/

	/**
	*	@param rows The number of rows
	*	@param cols The number of columns
	*	@param pInside The probability of a black pixel to be black
	*	@param pOutside The probability of a white pixel to be white
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix with blocks in a banded fashion
	*/
	def bandedBlocks(rows: Int, cols: Int, pInside: Double = 1.0, pOutside: Double = 1.0, convolutionSettings:ConvolutionSettings) = {

		val random = new scala.util.Random(SEED)
		val mat = DenseMatrix.fill[Double](rows,cols)(if(random.nextDouble < pOutside) 0.0 else 1.0)

		def makeSquare(x: Int, y: Int, side: Int, pInside: Double = 1.0) = {
			for(r <- Math.max(x-3,0) until Math.min(x+side, rows-1); c <- Math.max(y-3,0) until Math.min(y+side, cols-1)){
				mat(r,c) = if(random.nextDouble < pInside) 1.0 else 0.0
			}
		}

		val numblocks = 5
		val sizeBlock = rows/numblocks

		(0 until 10).foreach{ i =>
			makeSquare(i*sizeBlock, i*sizeBlock, sizeBlock, pInside)
		}

		BinarySparseMatrix(mat,desc=s"bandedBlocks($pInside;$pOutside)", convolutionSettings = convolutionSettings)

	}

	/**
	*	@param rows The number of rows
	*	@param cols The number of columns
	*	@param fat The width of the band
	*	@param pInside The probability of a black pixel to be black
	*	@param pOutside The probability of a white pixel to be white
	*	@param convolutionSettings The convolution settings to be used
	*	@return A "fat" binary diagonal matrix
	*/
	def generateBandedMatrix(rows:Int, cols:Int, fat:Int = 3, pInside: Double = 1.0, pOutside: Double = 1.0, convolutionSettings:ConvolutionSettings) = {
		val random = new scala.util.Random(SEED)
		val mat = DenseMatrix.tabulate[Double](rows,cols){case(i,j) =>
			if(Math.abs(i-j) < fat){
				if(random.nextDouble < pInside) 1.0 else 0.0
			}
			else{
				if(random.nextDouble < pOutside) 0.0 else 1.0
			}
		}

		BinarySparseMatrix(mat,desc=s"banded($pInside;$pOutside)", convolutionSettings = convolutionSettings)
	}

	/**
	*	@param rows The number of rows
	*	@param cols The number of columns
	*	@param pInside The probability of a black pixel to be black
	*	@param pOutside The probability of a white pixel to be white
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix composed containing blocks of ones
	*/
	def generateBlocksMatrix(rows:Int=300, cols:Int=300, pInside: Double = 1.0, pOutside: Double = 1.0, convolutionSettings:ConvolutionSettings):BinarySparseMatrix = {
		val random = new scala.util.Random(SEED)

		// Generate noisy matrix
		val mat = DenseMatrix.tabulate[Double](rows,cols){case(i,j) =>
			if(random.nextDouble < pOutside) 0.0 else 1.0
		}

		// Fill some parts with rectangles(top left, bottom right)

		import scala.collection.mutable.ArrayBuffer
		var rectangleList = ArrayBuffer[Rectangle]()

		/*	First rectangle  */
		val topLeft1 = Coord((0.1*rows).toInt, (0.1*cols).toInt)
		val bottomRight1 = Coord(topLeft1.row + (0.2*rows).toInt, topLeft1.col + (0.3*cols).toInt)
		rectangleList += Rectangle(topLeft1, bottomRight1)

		/*	Second rectangle  */
		val topLeft2 = Coord((0.2*rows).toInt, (0.25*cols).toInt)
		val bottomRight2 = Coord(topLeft2.row + (0.4*rows).toInt, topLeft2.col + (0.5*cols).toInt)
		rectangleList += Rectangle(topLeft2, bottomRight2)

		/*	Third rectangle  */
		val topLeft3 = Coord((0.5*rows).toInt, (0.1*cols).toInt)
		val bottomRight3 = Coord(topLeft3.row + (0.2*rows).toInt, topLeft3.col + (0.2*cols).toInt)
		rectangleList += Rectangle(topLeft3, bottomRight3)

		/*	Fourth rectangle  */
		val topLeft4 = Coord((0.65*rows).toInt, (0.8*cols).toInt)
		val bottomRight4 = Coord(topLeft4.row + (0.2*rows).toInt, topLeft4.col + (0.1*cols).toInt)
		rectangleList += Rectangle(topLeft4, bottomRight4)


		rectangleList.foreach{ rect =>
			rect.toRowRange.foreach{ r =>
				rect.toColRange.foreach{ c =>
					mat(r,c) = if(random.nextDouble < pInside) 1.0 else 0.0
				}
			}
		}

		BinarySparseMatrix(mat,desc=s"blocks($pInside;$pOutside)", convolutionSettings = convolutionSettings)
	}

	/**
	*	@param rows The number of rows
	*	@param cols The number of columns
	*	@param pInside The probability of a black pixel to be black
	*	@param pOutside The probability of a white pixel to be white
	*	@param convolutionSettings The convolution settings to be used
	*	@return A nested binary matrix
	*/
	def createTriangle(rows: Int, cols: Int, height: Int, width: Int, pInside: Double = 1.0, pOutside: Double = 1.0, convolutionSettings:ConvolutionSettings) = {
		val step = height/width.toDouble
		val random = new scala.util.Random(SEED)
		val mat = DenseMatrix.tabulate[Double](rows,cols){case(i,j) =>
			if(i < height - (j*step)){
				if(random.nextDouble < pInside) 1.0 else 0.0
			}
			else{
				if(random.nextDouble < pOutside) 0.0 else 1.0
			}
		}

		BinarySparseMatrix(mat,desc=s"triangle($pInside;$pOutside)", convolutionSettings = convolutionSettings)

	}

	/**
	*	@param p The probability for a given pixel to be white
	*	@param rows The number of rows
	*	@param cols The number of columns
	*	@param convolutionSettings The convolution settings to be used
	* 	@return A random noisy matrix
	*/
	def createRandomNoise(rows:Int=300, cols:Int = 300, p:Double, convolutionSettings:ConvolutionSettings) = {
		val random = new scala.util.Random(SEED)
		val mat = DenseMatrix.tabulate[Double](rows,cols){case(i,j) =>
			if(random.nextDouble < p) 0.0 else 1.0
		}
		BinarySparseMatrix(mat,desc=f"noise($p%.2f)", convolutionSettings = convolutionSettings)
	}

	/**
	*	@param rows The number of rows
	*	@param cols The number of columns
	*	@param x The row of the bottom left of the rectangle
	*	@param y The col of the bottom left of the rectangle
	*	@param width The width of the rectangle
	*	@param height The height of the rectangle 
	*	@param nBinaryMatrices The number of categories
	*	@param convolutionSettings The convolution settings to be used
	*	@return A categorical matrix containing a rectangle
	*/
	def createCatRectangle(rows: Int, cols: Int, x: Int, y:Int,width: Int, height: Int, nBinaryMatrices:Int, convolutionSettings:ConvolutionSettings) = {
		val random = new scala.util.Random(SEED)
		val mat = DenseMatrix.tabulate[Double](rows,cols){case(i,j) =>
			if(x <= i && i < x+width && y <= j && j < y+height) (math.floor((j-y).toDouble/(height.toDouble/nBinaryMatrices))).toInt else random.nextInt(nBinaryMatrices)
		}

		val bm = BinarySparseMatrix(mat,desc="catRectangle", convolutionSettings = convolutionSettings)
		assert(nBinaryMatrices == bm.nBinaryMatrices, s"nBinaryMatrices($nBinaryMatrices) != bm.nBinaryMatrices(${bm.nBinaryMatrices})")
		assert(if(nBinaryMatrices > 1) bm.isCat else !bm.isCat, s"The matrix is ${if(!bm.isCat) "not" else ""} categorical, but it should ${if(nBinaryMatrices == 1) "not" else ""} be")

		bm
	}

	/************************
	*		BACKUPS
	*************************/

	/** Loads a file that was saved using the writeToFile method from MyMatrixLike
	*
	*	@param A filename. The extension .backup will be added
	*	@param convolutionSettings The convolution settings to be used
	*	@return A matrix containing the data from the backup
	*/
	def loadFromBackup(filename:String, convolutionSettings:ConvolutionSettings):MatrixMoves = {
		val iterator = scala.io.Source.fromFile(filename).getLines

		val desc = iterator.next()
		val Array(rows, cols, nBinaryMatrices, dontChangeColorsInt) = iterator.next().split(" ").map(_.toInt)

		val dontChangeColors = dontChangeColorsInt==1

		val realMappedRows = iterator.next().split(" ").map(_.toInt)
		val realMappedCols = iterator.next().split(" ").map(_.toInt)

		assert(realMappedRows.length == rows, s"Corrupted backup realMappedRows, ${realMappedRows.length} != $rows")
		assert(realMappedCols.length == cols, s"Corrupted backup realMappedCols, ${realMappedCols.length} != $cols")

		val labelX = iterator.next().split(" ").map(_.replaceAllLiterally("|%|", " "))
		var labelY = iterator.next().split(" ").map(_.replaceAllLiterally("|%|", " "))

		assert(labelX.length == cols, s"Corrupted backup labelX, ${labelX.length} != $cols")
		assert(labelY.length == rows, s"Corrupted backup labelY, ${labelY.length} != $rows")

		val originalSparseMatrix = Array.tabulate[BitSet](nBinaryMatrices){ _ =>
			val bitset = new BitSet(rows * cols)

			(0 until rows).foreach{r=>
				iterator.next().split(" ").filter(_ != "").map(_.toInt).foreach{c =>
					bitset += Utils.getHash(r,c, cols)
				}
			}
			iterator.next() // Skip one line
			bitset
		}

		val m = new BinarySparseMatrix(
			initRows=rows,
			initCols=cols,
			originalSparseMatrix=originalSparseMatrix,
			convolutionSettings=convolutionSettings,
			labelX=labelX,
			labelY=labelY,
			desc=desc
		)

		m.dontChangeColors = dontChangeColors
		m.permute(realMappedRows, mapping=false)
		m.t
		m.permute(realMappedCols, mapping=false)
		m.t
	}

	// List of datasets
	val datasets = List("mammals",
						"imdb",
						"mushrooms",
						"mushroomsCAT",
						"bandedBlocks",
						"nested",
						"banded",
						"blocks",
						"noise",
						"categorical",
						"foodmart",
						"paleo",
						"docGraph",
						"genes",
						"football"
						)

	val usesNoise = List("nested", "banded", "noise","blocks","bandedBlocks") // Datasets that can be parameterized using noise
	val editableSize = usesNoise ++ List("categorical") // Datasets that can be sized parameterized

	val documentGraphs = List("news_3cl_1.mat", "news_3cl_2.mat", "news_3cl_2.mat", "news_5cl_1.mat", "news_5cl_2.mat", "news_5cl_3.mat")

	case class Noise(inside:Double, outside:Double){
		require(inside <= 1.0 && inside >= 0.0)
		require(outside <= 1.0 && outside >= 0.0)
	}

	/**
	*	@param dataset A string representing the dataset. May be a .backup file
	*	@param noise The noise to use for artificial datasets Noise(black noise, white noise)
	*	@param shuffle Should the matrix be shuffled on its rows & columns ?
	*	@param dims The (rows, columns) of the dataset, for the artificial datasets
	*	@param convolutionSettings The convolution settings to be used
	*	@return The matrix representing the dataset
	*/
	def apply(dataset:String,convolutionSettings:ConvolutionSettings, noise:Noise = Noise(0.0, 0.0), shuffle:Boolean=true, dims:(Int, Int)=(300,300)): MatrixMoves = {
		var isBackup = false
		val d = dataset match {
			case "mammals" => 			loadMammals(convolutionSettings)
			case "mushrooms" => 		loadMushrooms(convolutionSettings)
			case "mushroomsCAT" => 		loadCategoricalMushrooms(convolutionSettings)
			case "imdb" => 				loadIMDB(convolutionSettings)
			case "bandedBlocks" => 		bandedBlocks(dims._1,dims._2, pInside= 1.0-noise.inside, pOutside = 1.0-noise.outside, convolutionSettings)
			case "nested" => 			createTriangle(dims._1,dims._2,dims._1,dims._2, pInside= 1.0-noise.inside, pOutside = 1.0-noise.outside, convolutionSettings)
			case "banded" => 			generateBandedMatrix(dims._1,dims._2,fat=60, pInside = 1.0-noise.inside, pOutside = 1.0-noise.outside,convolutionSettings)
			case "blocks" => 			generateBlocksMatrix(dims._1,dims._2, pInside = 1.0-noise.inside, pOutside = 1.0-noise.outside,convolutionSettings)
			case "categorical" => 		createCatRectangle(dims._1,dims._2,100,100,120,120,6,convolutionSettings)
			case "foodmart" => 			loadSpaceSparse("/Datasets/Foodmart/foodmartFIM.txt",convolutionSettings)
			case "paleo" =>				loadNOW(convolutionSettings)
			case "genes" => 			loadGenes(convolutionSettings)
			case "noise" => 			createRandomNoise(p=noise.inside, convolutionSettings=convolutionSettings)
			case "football" => 			loadFootball(convolutionSettings)
			case docGraph if docGraph.startsWith("news_") => loadDocumentGraph(docGraph, convolutionSettings)
			case filename => 			loadFromBackup(filename,convolutionSettings)
		}

		if(shuffle) d.shuffle.t.shuffle.t else d
	}


}
