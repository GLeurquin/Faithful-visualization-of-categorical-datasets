package thesis


import thesis.utils._
import thesis.dataLoading._
import java.io.File

import breeze.plot._

/**
*	This recomputes the rescaled matrix and saves it as a PNG under the save name of the backup,
*	For all .backup files in the provided folder
*/
object RedoPNG extends MatrixHelpers with App{

	val folder = "src/main/resources/redoPNG" // Change this to the folder needed

	val convoSettings = ConvolutionSettings(Kernels.gaussianBlur, ErrorType.Abs, Padding.Boundary)

	Utils.getListOfFiles(folder).filter{f => f.getName().endsWith(".backup")}.foreach{ f =>
		val figure = new Figure("hi")
		figure.visible=false
		val dataset = LoadDataset(f.getPath(),convoSettings, shuffle=false)
		val (kernelSize, agreementScore) = visualizeRescaled(     dataset,
		                                       height = 1000,
		                                       width = 1000,
		                                       rows = 300,
		                                       cols = 300,
		                                       padding = Padding.ZerosAndOnes,
		                                       oldFig = Some(figure)
		                                 )
		figure.saveas(f.getPath().stripSuffix(".backup") + ".png")
	}

}
