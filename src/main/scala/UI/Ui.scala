package thesis.UI

import thesis.orderings._
import thesis.dataLoading._
import thesis.utils._
import thesis.rectangles._

import breeze.linalg._
import swing._
import swing.event._
import scala.swing.BorderPanel.Position._
import scala.swing.ListView.Renderer
import java.awt.{ Color, Graphics2D }
import java.io._
import scala.collection.GenSeq
import javax.swing.BorderFactory

import java.awt.event._

/** User interface that allows to reorder the matrices using the orderings and convolution
*/
object UI extends SimpleSwingApplication with MatrixHelpers {

	val myLabelFont = new java.awt.Font ("Times New Roman", java.awt.Font.BOLD, 20);

	def getIntTextField(init:String, cols:Int=5, acceptMax:Boolean=false) = new TextField {
		text = init
		listenTo(keys)

		def isNumber(ch:Char) = ch >= '0' && ch <= '9'
		def acceptMaxValue(ch:Char) = acceptMax && (text == "" && ch == 'm')
		reactions += {
		  case e: KeyTyped => if (!isNumber(e.char) && !acceptMaxValue(e.char)) e.consume
		}

		columns = cols
	}

	def getFloatTextField(init:String, cols:Int=5) = new TextField {
		text = init
		listenTo(keys)

		def testValue(v:Double) = v <= 1 && v >= 0

		reactions += {
		  case e: KeyTyped => if (!e.char.isDigit && (e.char != '.' || text.contains('.')) || !testValue((text+e.char).toDouble)) e.consume
		}

		columns = cols
	}

	val progressBar = new ProgressBar(){
		indeterminate = false
		value = 0
		visible = false
	}

	val localSearch = "Local Search"
	val depthSearch = "Depth Search"
	val bandedUnidirectional = "BandedUnidirectional"
	val bandedBidirectional = "BandedBidirectional"
	val bandedBarycentric = "Barycentric"
	val bruteForce = "BruteForce"
	val kMax = "KMax"
	val needingMoves = List(localSearch, depthSearch)
	val needingStopButton = List(localSearch, depthSearch, bandedUnidirectional, bandedBidirectional, bandedBarycentric)
	val orderingList = List(localSearch,"TSP", "KMax", "Complete", "Blocks", depthSearch, "Spectral","Nested",bruteForce,bandedUnidirectional, bandedBidirectional,bandedBarycentric, "Heatmap")
	val orderingsNotUsingIterations = List("TSP", "Blocks", "Spectral","Nested","BruteForce","Heatmap","Complete")

	val movesList = Array(Move.SwapRectangles, Move.KMax, Move.Swap, Move.SwapAdjacent, Move.Relocate,Move.Reverse).map(x => MoveProba(x))
	val errorDropDown = new ComboBox(ErrorType.errorTypes){
		listenTo(selection)
		reactions += {
			case e:SelectionChanged => recomputeError()
		}
	}
	val paddingDropDown = new ComboBox(List(Padding.ZerosAndOnes, Padding.Boundary, Padding.Zero)){
		listenTo(selection)
		reactions += {
			case e:SelectionChanged => recomputeError()
		}
	}


	/***************************************
			ORDERING PANEL
	****************************************/

	val shuffleCheckBox = new CheckBox("Shuffle"){
		tooltip = "Shuffles the dataset"
		selected = true
	}

	val datasetDropDown = new ComboBox(LoadDataset.datasets:+("Choose backup")){
		listenTo(selection)
		reactions += {
			case e:SelectionChanged => {
				noisePanel.visible = LoadDataset.usesNoise.contains(this.selection.item)
				rowsColsSizePanel.visible = LoadDataset.editableSize.contains(this.selection.item)
			}
		}
	}

	val orderingDropDown = new ComboBox(orderingList){
		listenTo(selection)
		reactions += {
			case e:SelectionChanged => {
				val item = this.selection.item
				val isLocalSearch = needingMoves.contains(item)

				graphPanel.visible = needingStopButton.contains(item)

				List(labelMoves, movesListView, segmentLabel, segmentField, reactionFactorField, reactionFactorLabel, autostopLabel, autostopGradient).foreach{
					_.visible = isLocalSearch
				}

				depthStrideField.visible = item == depthSearch
				depthStrideLabel.visible = item == depthSearch

				val showIterations = !orderingsNotUsingIterations.contains(item)
				iterationField.visible = showIterations
				iterationsLabel.visible = showIterations
			}
		}
	}

	val movesListView = new ListView(movesList){
		selectIndices(0)
	}

	val iterationField = getIntTextField("10000", 6)
	val iterationsLabel = new Label("Iterations")

	val segmentField = getIntTextField("500",5)
	val segmentLabel = new Label("Segment size"){
		tooltip = "Weights will be adjusted after that number of iterations"
	}

	val reactionFactorField = getFloatTextField("0.6", 5)
	reactionFactorField.tooltip = "Must be between 0 and 1"
	val reactionFactorLabel = new Label("Reaction Factor"){
		tooltip = "If equal to 1, will ignore move weights of previous segment when updating for the next segment. If set to 0, the weights will not be adjusted."
	}

	val labelMoves = new Label("Moves"){
		font = myLabelFont
	}

	val depthStrideLabel = new Label("Depth stride")
	val depthStrideField = getIntTextField("100", 4)
	depthStrideField.visible = false
	depthStrideLabel.visible = false

	val nRowsField = getIntTextField("300", 5)
	val nColsField = getIntTextField("300", 5)

	val insideNoiseField = getFloatTextField("0.1", 5)
	val outsideNoiseField = getFloatTextField("0.1", 5)
	val noisePanel = new BoxPanel(Orientation.Horizontal){
		contents += new BoxPanel(Orientation.Vertical){
			contents += new Label("Black Noise")
			contents += insideNoiseField
		}
		contents += new BoxPanel(Orientation.Vertical){
			contents += new Label("White Noise")
			contents += outsideNoiseField
		}

		visible = false
	}

	val rowsColsSizePanel = new BoxPanel(Orientation.Horizontal){
		contents += new BoxPanel(Orientation.Vertical){
			contents += new Label("Rows")
			contents += nRowsField
		}
		contents += new BoxPanel(Orientation.Vertical){
			contents += new Label("Cols")
			contents += nColsField
		}
		visible = false
	}

	val autostopGradient = new TextField{
		text = "-0.001"
		listenTo(keys)

		reactions +=
		{
		  case e: KeyTyped => {
		  	if(!(text=="" && e.char == '-')){
			  	try {
			  		(text+e.char).toDouble
			  	}
			  	catch {
			  		case ex:NumberFormatException => e.consume
			  	}
		  	}
		  }
		}

		columns = 2
		tooltip = "If the gradient for a segment is greater than the limit, it will stop"
	}

	val autostopLabel = new Label("Autostop threshold"){
		tooltip = "If positive, autostop is disabled."
	}

	val orderingPanel = new FlowPanel {
		contents += new Label("Dataset"){
			font = myLabelFont
		}
		contents += new BoxPanel(Orientation.Vertical){
			contents += datasetDropDown
			contents += shuffleCheckBox
			contents += noisePanel
			contents += rowsColsSizePanel
		}
		contents += new Label("Ordering"){
			font = myLabelFont
		}
		contents += orderingDropDown
		contents += labelMoves
		contents += movesListView

		contents += new BoxPanel(Orientation.Vertical){
			contents += iterationsLabel
			contents += iterationField
			contents += segmentLabel
			contents += segmentField
			contents += reactionFactorLabel
			contents += reactionFactorField
			contents += autostopLabel
			contents += autostopGradient
			contents += depthStrideLabel
			contents += depthStrideField
		}

		border = BorderFactory.createLineBorder(Color.black)
	}

	/***************************************
			MESH PANEL
	****************************************/

	val meshCheckBox = new CheckBox("Mesh"){
		listenTo(keys)
		reactions += {
		  case e: ButtonClicked => meshPanel.visible = this.selected
		}
	}

	val meshIterationsField = getIntTextField("1",4)

	val meshRowsField = getIntTextField("20",4)
	val meshColsField = getIntTextField("20",4)

	val meshPanel:FlowPanel = new FlowPanel{
	    contents += new Label("Mesh iterations")
	    contents += meshIterationsField
	    contents += new Label("Row Divider")
	    contents += meshRowsField
	    contents += new Label("Col Divider")
	    contents += meshColsField

	    border = BorderFactory.createLineBorder(Color.black)
	    visible = meshCheckBox.selected
	}

	/***************************************
			KERNEL PANEL
	****************************************/

	val MAX_KERNEL_SIZE = 49

	val kernelReaction:Reactions.Reaction = {
		case e:SelectionChanged => {
			updateKernelData(getKernelType)
			// 1 is the position of the kernel in the panel
			kernelPanel.contents(1) = kernelValuesPanel(getKernelRowSize,getKernelColSize)
			kernelPanel.revalidate()
			recomputeError()
		}
	}

	val kernelRowSizeDropDown = new ComboBox((3 to MAX_KERNEL_SIZE by 2)){
		listenTo(selection)
		reactions += kernelReaction
	}

	val kernelColSizeDropDown = new ComboBox((3 to MAX_KERNEL_SIZE by 2)){
		listenTo(selection)
		reactions += kernelReaction
	}

	val kernelTypeDropDown = new ComboBox(List("Gaussian Blur Smooth", "Gaussian Blur Exp","Cross","Identity", "Custom")){
		listenTo(selection)
		reactions += kernelReaction
	}

	val kernelData = Array.tabulate[TextField](MAX_KERNEL_SIZE,MAX_KERNEL_SIZE){case (_,_) =>
		getIntTextField("0",2)
	}

	def getKernelOfSize(sizeRows:Int, sizeCols:Int) = {
		val startRow = (MAX_KERNEL_SIZE-sizeRows)/2
		val endRow = MAX_KERNEL_SIZE-startRow

		val startCol = (MAX_KERNEL_SIZE-sizeCols)/2
		val endCol = MAX_KERNEL_SIZE-startCol
		kernelData.slice(startRow, endRow).map(_.slice(startCol,endCol))
	}

	def getKernelRowSize:Int = kernelRowSizeDropDown.selection.item
	def getKernelColSize:Int = kernelColSizeDropDown.selection.item
	def getKernelType:String = kernelTypeDropDown.selection.item
	def getOrderingName = orderingDropDown.selection.item
	def getIterations = iterationField.text.toInt

	def kernelValuesPanel(kernelRows:Int, kernelCols:Int) = new ScrollPane(new GridPanel(kernelRows,kernelCols){
		val kernelOfGoodSize = getKernelOfSize(kernelRows,kernelCols)
		kernelOfGoodSize.foreach{kernelRow=>
			kernelRow.foreach{kernelCol =>
				contents += kernelCol
			}
		}

		border = BorderFactory.createLineBorder(Color.black)

	}){
		preferredSize = new java.awt.Dimension(200,200)
	}

	def updateKernelData(kernelType:String):Unit = {

		val currentRowSize = getKernelRowSize
		val currentColSize = getKernelColSize

		val startRow = (MAX_KERNEL_SIZE-currentRowSize)/2
		val endRow = MAX_KERNEL_SIZE-startRow

		val startCol = (MAX_KERNEL_SIZE-currentColSize)/2
		val endCol = MAX_KERNEL_SIZE-startCol

		if(kernelType == "Custom"){
			(startRow until endRow).foreach{ r =>
				(startCol until endCol).foreach{ c =>
					kernelData(r)(c).editable = true
				}
			}
			return
		}

		val m = kernelType match {
			case "Gaussian Blur Smooth" => Kernels.gaussianBlurSmoothNotNormalized(currentRowSize,currentColSize).matrix
			case "Gaussian Blur Exp" => Kernels.gaussianBlurExpNotNormalized(currentRowSize,currentColSize).matrix
			case "Identity" => Kernels.identity(currentRowSize, currentColSize).matrix
			case "Cross" => Kernels.crossNotNormalized(currentRowSize, currentColSize).matrix
		}

		(startRow until endRow).foreach{ r =>
			(startCol until endCol).foreach{ c =>
				kernelData(r)(c).text = m(r-startRow,c-startCol).toInt.toString
				kernelData(r)(c).editable = false
			}
		}
	}

	val kernelPanel:FlowPanel = new FlowPanel{
		contents += new BoxPanel(Orientation.Vertical){
			contents += new Label("Kernel")
			contents += kernelTypeDropDown
			contents += new FlowPanel{
				contents += kernelRowSizeDropDown
				contents += new Label("X")
				contents += kernelColSizeDropDown
			}
		}
		updateKernelData(getKernelType)
		contents += kernelValuesPanel(getKernelRowSize,getKernelColSize)


		contents += new BoxPanel(Orientation.Vertical){
			contents += new Label("Padding")
			contents += paddingDropDown
			contents += new Label("Error")
			contents += errorDropDown
		}
	}


	/*************************************
				RUN PANEL
	*************************************/

	val runButton = new Button("Keep state & Run"){
		tooltip = "Keep current state and run the algorithm"
		background = Color.GREEN
	}

	val loadButton = new Button("Load"){
		tooltip = "Load the dataset"
		background = Color.YELLOW
	}

	val saveButton = new Button("Save .backup"){
		tooltip = "Save the current matrix into a backup file"
		background = Color.ORANGE
	}

	val fileChooser = new FileChooser(new File("src/main/resources/Backups/")){
		fileSelectionMode = FileChooser.SelectionMode.FilesOnly
		multiSelectionEnabled = false
		fileFilter = new javax.swing.filechooser.FileFilter{
			def accept(f:File) = {
				f.getName().endsWith(".backup")
			}
			def getDescription() = ".backup"
		}
	}

	var meshedDataset:Option[List[QuadrillageHelper]] = None
	var orderedDataset:Option[MatrixMoves] = None

	var currentOrdering = getOrderingName
	var currentKernel = getKernel
	var currentMoves = getMoves(movesListView)
	var currentIterations = getIterations
	var isCurrentlyMesh = meshCheckBox.selected


	val errorLabel = new Label("Error")
	val currentErrorTextArea = new TextField{
		text = "N/A"
		editable = false
		columns = 8
	}

	def askVisualizeConfirmation(dataset:MatrixMoves):Boolean = {
		if(dataset.rows * dataset.cols > 300000){
			val result = Dialog.showConfirmation(gridPanel, "This dataset is big and might crash your system.\nYou should use rescale or zoom instead.\n\nAre you sure you want to continue ?", title="Visualize huge dataset")
			if(result != Dialog.Result.Yes) return false
			return true
		}
		return true
	}

	val visualizeButton = new Button{
		text = "Visualize"

		action = Action(text){
			if(isCurrentlyMesh){
				meshedDataset match {
					case Some(list) => list.foreach(_.visualizeMatrix)
					case None => Dialog.showMessage(gridPanel, "You must first press run mesh!", title="Error")
				}
			}
			else{
				if(orderedDataset.isEmpty) load()
				if(orderedDataset.nonEmpty){
					val dataset = orderedDataset.get
					val description = s"${dataset.desc}-$currentOrdering($currentIterations)-$currentKernel-${currentMoves.map(_.move).mkString("-")}"
					if(zoom.selected){
						val rectangle = getSubmatrixRect(dataset)
						if(!rectangle.isEmpty){
							dataset.subMatrix(rectangle.get)
							if(askVisualizeConfirmation(dataset)) visualize(dataset, description + s"-subM(${rectangle.get})")
							dataset.insertSubmatrixNoChange()
						}
					}
					else if(askVisualizeConfirmation(dataset)) visualize(dataset, description)
				}
				// else the user canceled the data loading
			}


		}
		tooltip = "Visualize the current state of the matrix. May fail on big matrices due to lack of memory."
		background = Color.CYAN
	}

	val rescaleButton = new Button{
		text = "Rescale"
		action = Action(text){
			if(isCurrentlyMesh){
				meshedDataset match {
					case Some(list) => list.foreach(_.visualizeMatrixRescaled)
					case None => Dialog.showMessage(gridPanel, "You must first press run mesh!", title="Error")
				}
			}
			else{
				orderedDataset match {
					case Some(dataset) => {
						val description = s"${dataset.desc}-$currentOrdering($currentIterations)-$currentKernel-${currentMoves.map(_.move).mkString("-")}"
						if(zoom.selected){
							val rectangle = getSubmatrixRect(dataset)
							if(!rectangle.isEmpty){
								dataset.subMatrix(rectangle.get)
								visualizeRescaled(dataset, description + s"-subM($rectangle)")
								dataset.insertSubmatrixNoChange()
							}
						}
						else visualizeRescaled(dataset, description)
					}
					case None => Dialog.showMessage(gridPanel, "You must first press run!", title="Error")
				}
			}
		}
		tooltip = "Visualize the current mesh representing the matrix"
		background = Color.CYAN
	}

	/*******************************
			SUBMATRIX
	*******************************/

	val subMatrixButton:CheckBox = new CheckBox("Submatrix"){
		listenTo(keys)

		reactions +=
		{
		  case e: ButtonClicked => rectanglePanel.visible = zoom.selected || this.selected
		}
	}

	val zoom:CheckBox = new CheckBox("Zoom"){
		listenTo(keys)

		reactions +=
		{
		  case e: ButtonClicked => {rectanglePanel.visible = this.selected || subMatrixButton.selected}
		}
	}

	val topLeftRowRect = getIntTextField("0",4)
	val topLeftColRect = getIntTextField("0",4)
	val bottomRightRowRect = getIntTextField("m",4,acceptMax=true)
	val bottomRightColRect = getIntTextField("m",4,acceptMax=true)

	val errorGraph = new CheckBox("Error graph"){
		selected = true
	}
	val probaGraph = new CheckBox("Proba graph")
	val improvementGraph = new CheckBox("Improvement graph")
	val progressCheckBox = new CheckBox("Show progress"){
		selected = false
		tooltip = "Visualize the updated rescaled matrix every segment size"
	}

	val graphPanel = new BoxPanel(Orientation.Vertical){
		contents += errorGraph
		contents += probaGraph
		contents += improvementGraph
		contents += progressCheckBox
		visible = true
	}

	val rectanglePanel = new BoxPanel(Orientation.Vertical){
		contents += new Label("Enter m to get the max row/col")
		contents += new FlowPanel{
			contents += new BoxPanel(Orientation.Vertical){
				contents += new Label("Top row: (max)")
				contents += bottomRightRowRect
				contents += new Label("Bottom row: (min)")
				contents += topLeftRowRect
			}

			contents += new BoxPanel(Orientation.Vertical){
				contents += new Label("Left col: (min)")
				contents += topLeftColRect
				contents += new Label("Right col: (max)")
				contents += bottomRightColRect
			}

		}
		visible = zoom.selected || subMatrixButton.selected
	}

	val subMatrixPanel = new FlowPanel{
		contents += new BoxPanel(Orientation.Vertical){
			contents += zoom
			contents += subMatrixButton
			contents += meshCheckBox
		}
		contents += rectanglePanel

		contents += graphPanel


		border = BorderFactory.createLineBorder(Color.black)
	}


	/*******************************
			THE BRAIN
	*******************************/


	var g:Option[TimeGraph] = None
	def progressUpdate(progress:Double, error:Double):Unit = {
		progressBar.value = (progress*100).toInt
		currentErrorTextArea.text = f"${error}%2.2f"
		if(errorGraph.selected)	g.get +=(0,error)
	}

	var cancel = false
	val cancelButton = new Button{
		text = "Stop"
		background = Color.RED
		action = Action(text){
			cancel = true
		}
		tooltip = "Stop the ordering now"
		visible = false
	}

	def isCanceled() = cancel

	val noGraphConfig = GraphConfig(false, false, false, false)

	def getOrdering(m: MatrixMoves, name:String, moves:Array[MoveProba]):MatrixOrdering = {
		name match {
			case `localSearch` => new LocalSearch(getIterations,moves,progressUpdate, isCanceled, getGraphConfig, segmentField.text.toInt, reactionFactorField.text.toDouble, autostopGradient.text.toDouble)
			case "TSP" => val distance = if(m.isCat) (m:DenseMatrix[Double], i:Int, j:Int) => Utils.distanceCat(m,i,j) else (m:DenseMatrix[Double], i:Int, j:Int) => Utils.distanceBinary(m,i,j)
							new TSPOrdering((m:DenseMatrix[Double], i:Int, j:Int) => Utils.distanceBinary(m,i,j), distance)
			case "Complete" => new CompleteOrdering()
			case "KMax" => new KMaxOrdering()
			case "Blocks" => new FullBlockOrdering()
			case `depthSearch` => new DepthSearch(isCanceled, new LocalSearch(getIterations,moves,progressUpdate, isCanceled, noGraphConfig, segmentField.text.toInt), depthStrideField.text.toInt)
			case "Spectral" => new SpectralOrdering(SpectralOrdering.perso)
			case "Nested" => new NestedOrdering()
			case "BruteForce" =>  new BruteForceImproved()
			case `bandedUnidirectional` => new BandedUnidirectional(getIterations,progressUpdate, isCanceled, getGraphConfig.showProgress)
			case `bandedBidirectional` => new BandedBidirectional(getIterations, (x:Double)=>if(x==1.0) 1.0 else -1.0,progressUpdate, isCanceled, getGraphConfig.showProgress)
			case `bandedBarycentric` => new BandedBarycentric(getIterations,progressUpdate, isCanceled, getGraphConfig.showProgress)
			case "Heatmap" => new Heatmap()
		}
	}

	def getDataset(convolutionSettings:ConvolutionSettings):Option[MatrixMoves] = {
		var datasetName = datasetDropDown.selection.item

		if(datasetName == "Choose backup"){
			val fileChooserResult = fileChooser.showDialog(gridPanel,"open")
			if(fileChooserResult == FileChooser.Result.Cancel) return None
			datasetName = fileChooser.selectedFile.getPath()
			println(s"You selected ${datasetName}")
		}

		if(datasetName == "docGraph"){
			val chooserResult = Dialog.showInput(gridPanel,message="Choose the document graph", title="Choose graph", entries=LoadDataset.documentGraphs, initial=LoadDataset.documentGraphs.head)
			if(chooserResult.isEmpty) return None

			datasetName = chooserResult.get
		}

		Some(LoadDataset(
			datasetName,convolutionSettings,
			LoadDataset.Noise(insideNoiseField.text.toFloat, outsideNoiseField.text.toFloat),
			shuffle=shuffleCheckBox.selected,
			dims = (nRowsField.text.toInt, nColsField.text.toInt)
		))
	}

	def getKernel:Kernel = {
		val arrayKernel = getKernelOfSize(getKernelRowSize,getKernelColSize)
		Kernels.normalize(Kernel(DenseMatrix.tabulate[Double](getKernelRowSize, getKernelColSize){arrayKernel(_)(_).text.toInt}, kernelTypeDropDown.selection.item))
	}

	def getMoves(listView:ListView[MoveProba]) = listView.peer.getSelectedIndices().map(i => movesList(i)).toArray

	def showAllButtons(visible:Boolean) = {
		runButton.visible = visible
		loadButton.visible = visible
		saveButton.visible = visible
	}

	def getSubmatrixRect(dataset:MatrixMoves):Option[thesis.rectangles.Rectangle] = {
		try{
			val brr = if(bottomRightRowRect.text == "m") dataset.rows-1 else bottomRightRowRect.text.toInt.min(dataset.rows-1)
			val brc = if(bottomRightColRect.text == "m") dataset.cols-1 else bottomRightColRect.text.toInt.min(dataset.cols-1)
			val topLeft = Coord(topLeftRowRect.text.toInt.min(dataset.rows-1),topLeftColRect.text.toInt.min(dataset.cols-1))
			val bottomRight = Coord(brr,brc)

			val rectangle = Rectangle(topLeft, bottomRight)
			if(!rectangle.isDefinedInMatrix(dataset)){
				Dialog.showMessage(gridPanel, messageType=Dialog.Message.Error, message="The rectangle is not inside the matrix!", title="Invalid sub-matrix")
				return None
			}

			return Some(rectangle)
		}
		catch{
			case e:Exception => Dialog.showMessage(gridPanel, messageType=Dialog.Message.Error, message="The entered values are not valid!", title="Invalid sub-matrix")
		}
		None
	}

	def getGraphConfig = {
		GraphConfig(probaGraph.selected, improvementGraph.selected, errorGraph.selected, progressCheckBox.selected)
	}

	def recomputeError():Unit = {
		if(orderedDataset.isEmpty) return
		val convolutionSettings = ConvolutionSettings(getKernel, errorDropDown.selection.item, paddingDropDown.selection.item)
		val dataset = orderedDataset.get
		dataset.setConvolutionSettings(convolutionSettings)
		currentErrorTextArea.text = f"${dataset.getError}%2.2f"
	}

	def load(){
		val convolutionSettings = ConvolutionSettings(getKernel, errorDropDown.selection.item, paddingDropDown.selection.item)
		getDataset(convolutionSettings) match{
			case Some(dataset) => {
				orderedDataset = Some(dataset)
				currentErrorTextArea.text = f"${dataset.getError}%2.2f"

				val moves = getMoves(movesListView)
				// Reset move weights
				moves.foreach{_.probaWeight = 1.0}
				println(s"Dataset ${dataset.desc} has ${dataset.rows} rows and ${dataset.cols} columns")
			}
			case None => return
		}
	}

	def run(previousMatrix:Option[MatrixMoves], isMesh:Boolean){
		val moves = getMoves(movesListView)
		val orderingName = getOrderingName

		if(needingMoves.contains(orderingName) && moves.isEmpty){
			Dialog.showMessage(gridPanel, "You must select some moves for the ordering", title="Missing information")
			return
		}

		if(orderedDataset.isEmpty) load()
		if(orderedDataset.isEmpty) return // If dataset is still empty, user canceled the run
		var dataset = orderedDataset.get

		val ordering = getOrdering(dataset, orderingName, moves)

		val decision:Boolean = if(subMatrixButton.selected){
			val rectangleOption = getSubmatrixRect(dataset)
			if(rectangleOption.isEmpty) return

			dataset.subMatrix(rectangleOption.get)
			true
		}
		else false

		showAllButtons(false)
		progressBar.visible = true
		progressBar.value = 0
		if(needingStopButton.contains(orderingName)) {
			cancelButton.visible = true
			progressBar.indeterminate = false

			if(g.nonEmpty) g.get.dispose()
			if(errorGraph.selected) g = Some(new TimeGraph("Live Error", "Error",Array("Error")))
		}
		else{
			progressBar.indeterminate = true
		}

		cancel = false

		// Just for the description of the image
		currentOrdering = orderingName
		currentKernel = getKernel
		currentMoves = moves
		isCurrentlyMesh = isMesh

		val thread = new Thread {
        override def run {
        		if(isMesh){
        			val quadrillageOrdering = new QuadrillageOrdering(meshIterationsField.text.toInt, ordering, QuadrillageBuilder.score2, meshRowsField.text.toInt, meshColsField.text.toInt)
        			val newDatasets = quadrillageOrdering.applyMesh(dataset)
        			newDatasets.foreach{case QuadrillageHelper(m, r) => m.insertAllSubmatrix()}
					meshedDataset = Some(newDatasets)
        		}
        		else{
	            	dataset = ordering.changeOrdering(dataset)
	            	dataset.insertAllSubmatrix()
					orderedDataset = Some(dataset)

            		currentErrorTextArea.text = f"${dataset.getError}%2.2f"
	            	val usesIteration = !orderingsNotUsingIterations.contains(orderingName)
	            	if(usesIteration) {
	            		val iterationsPerformed = 	if(orderingName == localSearch) ordering.asInstanceOf[LocalSearch].it
	            									else getIterations
	            		if(previousMatrix.isEmpty) currentIterations = iterationsPerformed
	            		else currentIterations += iterationsPerformed
	            	}
	            	meshedDataset = None
        		}


				showAllButtons(true)

				cancelButton.visible = false
	    		progressBar.visible = false
	        }
	    }
	    thread.start
	}

	/***********************************
			MAIN PANEL
	***********************************/

	val gridPanel:BoxPanel = new BoxPanel(Orientation.Vertical) {
		contents += orderingPanel

		contents += kernelPanel

		contents += subMatrixPanel
		contents += meshPanel

		contents += new FlowPanel{
			contents += loadButton
			contents += runButton

			contents += saveButton
			contents += cancelButton
		}
		contents += new FlowPanel{
			contents += errorLabel
			contents += currentErrorTextArea
		}
		contents += new FlowPanel{
			contents += visualizeButton
			contents += rescaleButton
		}
		contents += progressBar
	}


	def top = new MainFrame {
		title = "Order a matrix"
		contents = new ScrollPane(gridPanel)

		size = new Dimension(1000, 700)

		menuBar = new MenuBar {
	      contents += new Menu("Menu") {
	        contents += new MenuItem(Action("Exit") {
	          sys.exit(0)
	        })
	      }
	    }
	}

	listenTo(loadButton,runButton,saveButton)

	reactions += {
		case ButtonClicked(`loadButton`) => {
			if(orderedDataset.isEmpty) load() // Skip dialog for the first time
			else {
				val result = Dialog.showConfirmation(gridPanel, "Are you sure ? You will loose current progress", title="Load confirmation")
				if(result == Dialog.Result.Yes) load()
			}
		}
		case ButtonClicked(`runButton`) => run(None, isMesh=meshCheckBox.selected)
		case ButtonClicked(`saveButton`) => {
			if(orderedDataset.isEmpty) Dialog.showMessage(gridPanel, "You must first press run!", title="Error")
			else{
				val filename = Dialog.showInput(gridPanel, "Name of the backup (without the .backup)", title="Save the matrix", initial=orderedDataset.get.desc)
			    filename match {
			      case Some(s) => {
			      	val fileToSave = s"src/main/resources/Backups/$s"
			      	orderedDataset.get.writeToFile(s"$fileToSave")
			      	Dialog.showMessage(gridPanel, s"File saved under $fileToSave.backup !", title="File saved !")
			      }
			      case None => Unit
			    }

			}
		}
	}

}
