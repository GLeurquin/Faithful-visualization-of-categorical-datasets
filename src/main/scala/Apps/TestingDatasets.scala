package thesis

import thesis.dataLoading._
import thesis.orderings._
import thesis.utils._

import breeze.plot._

import java.io.IOException
import java.io.File

import breeze.linalg._
import breeze.numerics._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.StringBuilder
import scala.collection.mutable.HashMap

/**
*    This class defines a series of ordering that can be used for testing
*/
object Tests{

    def chainOrderings(orderings:MatrixOrdering*):MatrixOrdering = {
          if(orderings.exists(_.isInstanceOf[UsesKernelOptimization])) new ChainOrdering(orderings: _*) with UsesKernelOptimization
          else new ChainOrdering(orderings: _*)
    }

     /*****************************
          Distance for the TSP
     *****************************/
     private val distanceCat = (m:DenseMatrix[Double], i:Int, j:Int) => Utils.distanceCat(m,i,j)
     private val distanceBin = (m:DenseMatrix[Double], i:Int, j:Int) => Utils.distanceBinary(m,i,j)

     def tspCat = new TSPOrdering((m:DenseMatrix[Double], i:Int, j:Int) => Utils.distanceBinary(m,i,j), distanceCat)
     def tspBin = new TSPOrdering((m:DenseMatrix[Double], i:Int, j:Int) => Utils.distanceBinary(m,i,j), distanceBin)

     def nestedOrdering = new NestedOrdering()
     def barycentric = new BandedBarycentric(10)
     def bidirectional = new BandedBidirectional(10)
     def heatmap = new Heatmap()

     def lsAll = new LocalSearch(MAXIT = 200000,
          moves = Array(Move.SwapRectangles, Move.Swap, Move.SwapAdjacent, Move.Relocate,Move.Reverse).map(x => MoveProba(x)),
          segment_length = 100,
          reaction_factor = 0.6,
          autostopGradient = -0.0001
     )

     def kMax = new KMaxOrdering(k=25)

     def lsKmax = new LocalSearch(MAXIT = 200000,
          moves = Array(Move.KMax).map(x => MoveProba(x)),
          segment_length = 100,
          reaction_factor = 0.6,
          autostopGradient = -0.0001
     )

     def lsAllNoUpdate = new LocalSearch(MAXIT = 200000,
          moves = Array(Move.SwapRectangles, Move.Swap, Move.SwapAdjacent, Move.Relocate,Move.Reverse).map(x => MoveProba(x)),
          segment_length = 500,
          reaction_factor = 0.6,
          autostopGradient = -0.0001,
          moveUpdate = false
     )

     def complete = new CompleteOrdering()

     def lsMove(move:Move) = new LocalSearch(MAXIT = 200000,
          moves = Array(move).map(x => MoveProba(x)),
          segment_length = 500,
          reaction_factor = 0.6,
          autostopGradient = -0.0001
     )
     val moves = List(Move.SwapRectangles, Move.Swap,Move.SwapAdjacent,Move.Relocate,Move.Reverse)
     def lsMoves = moves.map{m => lsMove(m)}

     val artificial = List("nested", "banded", "blocks", "bandedBlocks")
}

/**
*    @param experience : name of the experiment, and of the folder in which they will be saved
*    Orderings will be reused if they have already been run for some particular settings. This is determined based
*    On the string representation of the ordering, so make sure to include all the parameters of the ordering in its string representation
*/
class TestingConfig(val experience:String) {
     var orderings = List[MatrixOrdering]()
     var configs = List[ConvolutionSettings]()
     var datasets = List[String]()
     var noises = List[LoadDataset.Noise]()

     def +=[R](v:R):this.type = {
          v match {
               case r:MatrixOrdering => orderings = orderings ++ List(r)
               case r:ConvolutionSettings => configs = configs ++ List(r)
               case r:LoadDataset.Noise => noises = noises ++ List(r)
               case r:String => datasets = datasets ++ List(r)
          }
          this
     }

     def ++=[R](v:List[R]):this.type = {
          v match {
               case r if r.forall(_.isInstanceOf[MatrixOrdering]) => orderings = orderings ++ r.map(_.asInstanceOf[MatrixOrdering])
               case r if r.forall(_.isInstanceOf[ConvolutionSettings]) => configs = configs ++ r.map(_.asInstanceOf[ConvolutionSettings])
               case r if r.forall(_.isInstanceOf[LoadDataset.Noise]) => noises = noises ++ r.map(_.asInstanceOf[LoadDataset.Noise])
               case r if r.forall(_.isInstanceOf[String]) => datasets = datasets ++ r.map(_.asInstanceOf[String])
          }
          this
     }
}

/**
*    This class will run all combinations of settings contained in the testingConfig and write the result into a latex file
*    It will also generate PNG files of the results as well as .backup files to reload them if need be
*/
class TestingDatasets(testingConfig:TestingConfig) extends MatrixHelpers{

     case class CacheEntry(backupName:String, time:Long)

     def run {

          /*********************************
          *         Testing Config
          **********************************/

          val orderings = testingConfig.orderings
          val configs = testingConfig.configs
          val datasets = testingConfig.datasets
          val noises = testingConfig.noises
          val experience = testingConfig.experience.replaceAllLiterally(" ", "") // no spaces allowed

          val cache = new HashMap[String,CacheEntry]() // ordering-noise-(convoSettings if UsesKernelOptimization) --> (backup filename, time taken)

          def checkCache[R](dataset:String, ordering:MatrixOrdering, noise:LoadDataset.Noise, convoSettings:ConvolutionSettings)(block:((String,MatrixOrdering,Long) => R)) = {
               val isChain = ordering.isInstanceOf[ChainOrdering]
               val firstOrdering = if(isChain) ordering.asInstanceOf[ChainOrdering].firstOrdering // Select only the first ordering
                                   else ordering
                    
               val newMatConfig:String = s"$dataset-$firstOrdering-$noise" + (if(firstOrdering.isInstanceOf[UsesKernelOptimization]) s"-convoSettings" else "")
               
               val CacheEntry(filename, time) = if(cache.contains(newMatConfig)) {
                    println(s"Reusing $dataset for $newMatConfig")
                    cache(newMatConfig)
               }
               else {
                    // Run the ordering:
                    val data = LoadDataset(dataset, convoSettings, noise, shuffle=true)
                    val Utils.TimeResult(orderedMatrix,time) = Utils.time(firstOrdering.changeOrdering(data))
                    
                    val filename = Utils.sanitizeFilename(s"src/main/resources/Backups/$dataset-$newMatConfig") + ".backup"
                    orderedMatrix.writeToFile(filename)

                    val entry = CacheEntry(filename, time)
                    cache(newMatConfig) = entry
                    println(s"Saved $dataset for $newMatConfig in cache !")
                    entry
               }

               if(isChain) block(filename, ordering.asInstanceOf[ChainOrdering].skipFirst, time) // remove first ordering from the chain
               else block(filename, new Identity(), time)
          }

          def cleanupCache(){
               cache.values.foreach{f => 
                    (new java.io.File(f.backupName)).delete()
               }
          }

          /*********************************
          *    Directory in which to save the images
          *    Will be created if it does not exist
          *           /!\ ATTENTION: /!\
          *    The directory (and all its contents)
          *    will be deleted prior to generating the images
          **********************************/
          println(s"Running experience $experience ...")
          val directory = s"src/main/resources/$experience"

          /*********************************
          *    Delete all previous images
          **********************************/
          def deleteRecursively(file: File): Unit = {
              if (file.isDirectory)
                file.listFiles.foreach(deleteRecursively)
              if (file.exists && !file.delete)
                throw new IOException(s"Unable to delete ${file.getAbsolutePath}")
          }

          val file = new File(directory)
          deleteRecursively(file)


          val dir = new File(directory)
          // attempt to create the directory here
          if(!dir.mkdir()){
               throw new IOException(s"Unable to create the directory ${dir.getAbsolutePath}")
          }

          // rows correspond to datasets, and the columns to algorithms


          /*********************************
          *         Run Config
          **********************************/

          case class TestResult(dataset:String, figName:String, time:Long, noiseConfig:LoadDataset.Noise, errors:Map[ConvolutionSettings, Double])

          def getErrors(data:MatrixMoves) = {
               configs.map{ c =>
                    data.setConvolutionSettings(c)
                    (data.getConvolutionSettings,data.getError)
               }.toMap
          }

          val defaultSettings = configs.head

          def getOrderingFigure(datasetDesc:String, dataset:String, convoSettings:ConvolutionSettings, noiseConfig:LoadDataset.Noise, ordering:MatrixOrdering, previousTime:Long, info:String, errors:(MatrixMoves)=>Map[ConvolutionSettings, Double]) = {
               // Load the dataset
               val data = LoadDataset(dataset, convoSettings, noiseConfig, shuffle=false)
               val Utils.TimeResult(orderedMatrix,tempTime) = Utils.time(ordering.changeOrdering(data))
               val time =     if(ordering.isInstanceOf[Identity]) previousTime
                              else tempTime + previousTime
               val localSearchDesc = if(ordering.isInstanceOf[UsesKernelOptimization]) f"E(${orderedMatrix.getError}%.0f)-"
                                     else ""
               val figName = Utils.save(orderedMatrix, f"$directory/${orderedMatrix.desc}-${localSearchDesc}-${time}ms-$info").stripPrefix(s"$directory/")

               TestResult(datasetDesc, figName, time, noiseConfig, errors(orderedMatrix))
          }

          val numberOfFiguresOnARow = 1+ 2 + orderings.map{x => if(x.isInstanceOf[UsesKernelOptimization]) configs.length else 1}.sum
          val widthRatio = 1.0/numberOfFiguresOnARow

          val numberOfRowsOfFigures = 4

          val figureHeight = "30mm"

          val tableHeader = s"""
     \\newgeometry{margin=\\theMargin} % modify this if you need even more space
     \\begin{landscape}
     \\begin{table}[h!]
          \\centering
          \\begin{tabular}{ ${{1 to numberOfFiguresOnARow}.map(_ => "c").mkString(" | ")} }
              \\hline
               Config & Original & Shuffled & ${orderings.map{x => if(x.isInstanceOf[UsesKernelOptimization]) configs.map(_ => "TSP + LS").mkString(" & ") else x}.mkString(" & ")} \\\\ \\hline

     """

          def tableFooter(noiseConfig:LoadDataset.Noise) = s"""
          \\end{tabular}
          \\caption{$experience - ${noiseConfig}}
     \\end{table}
     \\end{landscape}
     \\restoregeometry
     """

          def getErrorFormatted(testResult:TestResult, config:ConvolutionSettings) = {
               testResult.errors.get(config) match {
                    case Some(error) => f"$error%.0f"
                    case None => "/"
               }
          }

          val configMap = configs.zipWithIndex.map{case(cfg, idx) =>
               (cfg -> s"C$idx")
          }.toMap

         def getImagePage(testResult:TestResult) = s"""
          \\begin{minipage}{\\figureWidth$experience \\textwidth}
               \\includegraphics[width=\\linewidth, height=\\figureHeight$experience]{Figs/Chapter5/$experience/${testResult.figName}.png}
          \\end{minipage}
          """


          val latexFile = new java.io.File(s"$directory/latex.tex")

          Utils.printToFile(latexFile){p =>
               p.println(s"\\newcommand{\\figureHeight$experience}{$figureHeight}")
               p.println(f"\\newcommand{\\figureWidth$experience%s}{$widthRatio%.3f}")

               p.println(configMap.mkString("\n\\begin{itemize}\\item ", "\\item ", "\\end{itemize}"))
          }

          def printProgress(currentFigName:TestResult, p:java.io.PrintWriter) = {
               p.print("\n     &\n")
               p.print(getImagePage(currentFigName))

               p.print("\n")
               configs.foreach{config =>
                    p.print("% ")
                    p.print(configMap(config))
                    p.print(" : ")
                    p.print(getErrorFormatted(currentFigName, config))
                    p.print("\n")
               }

               currentFigName
          }

          noises.foreach{ noiseConfig =>

               var i = 0
               datasets.foreach{ dataset =>

                    val figNames = new ArrayBuffer[TestResult]()

                    /**
                    *    Compute the original and shuffled datasets
                    */
                    Utils.printToFile(latexFile, append=true){p =>
                         if(i % numberOfRowsOfFigures == 0) p.print(tableHeader)
                         p.print("\n")
                         p.print(dataset)

                         val nonShuffled = LoadDataset(dataset, defaultSettings, noiseConfig, shuffle=false)
                         figNames += printProgress(TestResult(dataset, Utils.save(nonShuffled, s"$directory/${nonShuffled.desc}-Original").stripPrefix(s"$directory/"), 0, noiseConfig, getErrors(nonShuffled)), p)

                         val initShuffled = LoadDataset(dataset, defaultSettings, noiseConfig)
                         figNames += printProgress(TestResult(dataset, Utils.save(initShuffled, s"$directory/${initShuffled.desc}-Shuffled").stripPrefix(s"$directory/"), 0, noiseConfig, getErrors(initShuffled)), p)
                    }

                    /**
                    *    Compute the orderings
                    */
                    orderings.foreach{ ordering =>
                         if(ordering.isInstanceOf[UsesKernelOptimization]){
                             // run on each config
                              configs.foreach{ convoSettings =>
                                   Utils.printToFile(latexFile, append=true){p =>
                                        checkCache(dataset, ordering, noiseConfig ,convoSettings){ case(cachedDataset, newOrdering, time) => 
                                             figNames += printProgress(getOrderingFigure(dataset, cachedDataset, convoSettings, noiseConfig, newOrdering,time, info=s"-$convoSettings-$ordering", (m:MatrixMoves) => Map(m.getConvolutionSettings -> m.getError)),p)
                                        }
                                   }
                              }
                         }
                         else{
                             // compute the error for each config
                              Utils.printToFile(latexFile, append=true){p =>
                                   checkCache(dataset, ordering, noiseConfig,defaultSettings){  case(cachedDataset, newOrdering, time)  => 
                                        figNames += printProgress(getOrderingFigure(dataset, cachedDataset, defaultSettings, noiseConfig, newOrdering, time, info=s"-$ordering", getErrors),p)
                                   }
                              }
                         }
                    }

                    /**
                    *    Save to latex file
                    */
                    Utils.printToFile(latexFile, append=true){p =>
                         p.print("\\\\") // End current line
                         p.print("\n") // Go to new line

                         // Lines for the errors
                         configs.foreach{config =>
                              p.print(configMap(config))
                              p.print(" & ")
                              p.print(figNames.map{r => getErrorFormatted(r, config)}.mkString(""," & ","\\\\"))
                              p.print("\n")
                         }

                         p.print("Time(s) & ")
                         p.print(figNames.map(r => f"${r.time/1000.0}%.2f").mkString(""," & ","\\\\\n"))

                         if((i+1) % numberOfRowsOfFigures == 0) {
                              p.print(tableFooter(noiseConfig))
                              p.println("\n\n\n")
                         }
                    }


                    i += 1
               }

               /**
               *    Print latex footer
               */
               Utils.printToFile(latexFile, append=true){p =>
                    if(i % numberOfRowsOfFigures != 0) {
                         // Footer was not printer before
                         p.print(tableFooter(noiseConfig))
                         p.println("\n\n\n")
                    }
               }
          }


          cleanupCache()
     }

}
