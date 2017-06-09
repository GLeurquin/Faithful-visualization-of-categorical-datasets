package thesis

import thesis.dataLoading._
import thesis.orderings._
import thesis.utils._

import java.io.IOException

object CreateHeatMaps extends App {
	val folder = "src/main/resources/toR" // Change this to the folder needed

	val configs = Array(
		ConvolutionSettings(Kernels.gaussianBlurSmooth(49,49), ErrorType.Abs, Padding.ZerosAndOnes)
	) 


	Utils.getListOfFiles(folder).filter{f => f.getName().endsWith(".backup")}.filter(!_.getName().contains("heatmap")).foreach{ f =>
		val heatmapOrdering = new Heatmap(f.getParent() + "/")
		val m = LoadDataset(f.getPath(),configs.head, shuffle=false)
		// m.writeMatrixAsRInput(f.getPath().stripSuffix(".backup")) // Just to write the rformat

		val Utils.TimeResult(result, time) = Utils.time(heatmapOrdering.changeOrdering(m))


		Utils.save(m, f.getPath().stripSuffix(".backup")+s"-heatmap-${time}ms")

		/* Print the configs */
		try{
			Utils.printToFile(new java.io.File(f.getPath().stripSuffix(".backup") + ".configs")){ p => 
				configs.foreach{c => 
					m.setConvolutionSettings(c)
					val e = m.getError
					p.print(c)
					p.print(" ")
					p.println(f"$e%.0f")
				}
			}
		}
		catch{
			case e:IOException => println("Seems like something failed...")
		}
	}
}