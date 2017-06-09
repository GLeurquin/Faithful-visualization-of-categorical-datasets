package thesis

import breeze.linalg._
import thesis.dataLoading._

import scala.collection.mutable.ArrayBuffer

object RunTesting extends App{
    /******************************
           CONFIGS         
    ******************************/
    val testingConfigs = new ArrayBuffer[TestingConfig]()

    val noise = LoadDataset.Noise(0.25,0.25)

    // testingConfigs += (new TestingConfig("MushCat",Tests.tspCat))
    //     .+= (noise)
    //     .+= ("mushroomsCAT")
    //     .+= (new HeatmapOrdering("src/main/resources/MushCat/"))
    //     .+= (Tests.tspCat)
    //     .+= (Tests.complete)
    //     .++= (List(
    //         Kernels.gaussianBlurSmooth(3,3),
    //         Kernels.gaussianBlurSmooth(11,3),
    //         Kernels.gaussianBlurSmooth(11,11)
    //     ).flatMap{ k =>
    //         List(ErrorType.Abs).map{e =>
    //           new ConvolutionSettings(kernel=k, errorType=e,padding=Padding.ZerosAndOnes)
    //     }
    // })


    // testingConfigs += (new TestingConfig("MushCat21",Tests.tspCat))
    //     .+= (noise)
    //     .+= ("mushroomsCAT")
    //     // .+= (new HeatmapOrdering("src/main/resources/MushCat21/"))
    //     // .+= (Tests.tspCat)
    //     .+= (Tests.complete)
    //     .++= (List(
    //         Kernels.gaussianBlurSmooth(21,21)
    //     ).flatMap{ k =>
    //         List(ErrorType.Abs).map{e =>
    //           new ConvolutionSettings(kernel=k, errorType=e,padding=Padding.ZerosAndOnes)
    //     }
    // })



    // testingConfigs += (new TestingConfig("initSolNestedBanded"))
    //     .+= (noise)
    //     .+= ("nested")
    //     .+= ("banded")
    //     .+= (new HeatmapOrdering("src/main/resources/initSolNestedBanded/"))
    //     .+= (Tests.nestedOrdering)
    //     .+= (Tests.barycentric)
    //     .+= (Tests.bidirectional)
    //     .+= (Tests.tspBin)
    //     .+= (Tests.chainOrderings(new HeatmapOrdering("src/main/resources/initSolNestedBanded/"),Tests.lsAll))
    //     .+= (Tests.chainOrderings(Tests.nestedOrdering,Tests.lsAll))
    //     .+= (Tests.chainOrderings(Tests.barycentric,Tests.lsAll))
    //     .+= (Tests.chainOrderings(Tests.bidirectional,Tests.lsAll))
    //     .+= (Tests.chainOrderings(Tests.tspBin,Tests.lsAll))
    //     .++= (List(
    //         Kernels.gaussianBlurSmooth(25,25)
    //     ).flatMap{ k =>
    //       List(ErrorType.Abs).map{e =>
    //            new ConvolutionSettings(kernel=k, errorType=e,padding=Padding.ZerosAndOnes)
    //       }
    //     })

    // testingConfigs += (new TestingConfig("initSolBlocks"))
    //     .+= (noise)
    //     .+= ("blocks")
    //     .+= ("bandedBlocks")
    //     .+= (new HeatmapOrdering("src/main/resources/initSolBlocks/"))
    //     .+= (Tests.nestedOrdering)
    //     .+= (Tests.barycentric)
    //     .+= (Tests.bidirectional)
    //     .+= (Tests.tspBin)
    //     .+= (Tests.chainOrderings(new HeatmapOrdering("src/main/resources/initSolBlocks/"),Tests.lsAll))
    //     .+= (Tests.chainOrderings(Tests.nestedOrdering,Tests.lsAll))
    //     .+= (Tests.chainOrderings(Tests.barycentric,Tests.lsAll))
    //     .+= (Tests.chainOrderings(Tests.bidirectional,Tests.lsAll))
    //     .+= (Tests.chainOrderings(Tests.tspBin,Tests.lsAll))
    //     .++= (List(
    //         Kernels.gaussianBlurSmooth(25,25)
    //     ).flatMap{ k =>
    //       List(ErrorType.Abs).map{e =>
    //            new ConvolutionSettings(kernel=k, errorType=e,padding=Padding.ZerosAndOnes)
    //       }
    //     })


    // testingConfigs += (new TestingConfig("kmaxFive"))
    //     .+= (noise)
    //     .++= (Tests.artificial)
    //     .+= (Tests.kMax)
    //     .++= (List(
    //         Kernels.gaussianBlurSmooth(5,5)
    //     ).flatMap{ k =>
    //       List(ErrorType.Abs).map{e =>
    //            new ConvolutionSettings(kernel=k, errorType=e,padding=Padding.ZerosAndOnes)
    //       }
    //     })

    // testingConfigs += (new TestingConfig("mushroomsHMBidi"))
    //     .+= (noise)
    //     .+= ("mushrooms")
    //     .+= (Tests.heatmap)
    //     .+= (Tests.chainOrderings(Tests.heatmap,Tests.complete))
    //     .+= (Tests.bidirectional)
    //     .+= (Tests.chainOrderings(Tests.bidirectional,Tests.complete))
    //     .++= (List(
    //         Kernels.gaussianBlurSmooth(25,25)
    //     ).flatMap{ k =>
    //       List(ErrorType.Abs).map{e =>
    //            new ConvolutionSettings(kernel=k, errorType=e,padding=Padding.ZerosAndOnes)
    //       }
    //     })

    // testingConfigs += (new TestingConfig("mammalsHMBidi"))
    //     .+= (noise)
    //     .+= ("mammals")
    //     .+= (Tests.heatmap)
    //     .+= (Tests.chainOrderings(Tests.heatmap,Tests.complete))
    //     .+= (Tests.bidirectional)
    //     .+= (Tests.chainOrderings(Tests.bidirectional,Tests.complete))
    //     .++= (List(
    //         Kernels.gaussianBlurSmooth(25,25)
    //     ).flatMap{ k =>
    //       List(ErrorType.Abs).map{e =>
    //            new ConvolutionSettings(kernel=k, errorType=e,padding=Padding.ZerosAndOnes)
    //       }
    //     })

    // testingConfigs += (new TestingConfig("initSolNestedBanded"))
    //     .+= (noise)
    //     .+= ("nested")
    //     .+= ("banded")
    //     .+= (Tests.heatmap)
    //     .+= (Tests.chainOrderings(Tests.heatmap,Tests.complete))
    //     .+= (Tests.nestedOrdering)
    //     .+= (Tests.chainOrderings(Tests.nestedOrdering,Tests.complete))
    //     .+= (Tests.barycentric)
    //     .+= (Tests.chainOrderings(Tests.barycentric,Tests.complete))
    //     .+= (Tests.bidirectional)
    //     .+= (Tests.chainOrderings(Tests.bidirectional,Tests.complete))
    //     .+= (Tests.tspBin)
    //     .+= (Tests.chainOrderings(Tests.tspBin,Tests.complete))
    //     .++= (List(
    //         Kernels.gaussianBlurSmooth(25,25)
    //     ).flatMap{ k =>
    //       List(ErrorType.Abs).map{e =>
    //            new ConvolutionSettings(kernel=k, errorType=e,padding=Padding.ZerosAndOnes)
    //       }
    //     })

    // testingConfigs += (new TestingConfig("initSolBlocks"))
    //     .+= (noise)
    //     .+= ("blocks")
    //     .+= ("bandedBlocks")
    //     .+= (Tests.heatmap)
    //     .+= (Tests.chainOrderings(Tests.heatmap,Tests.complete))
    //     .+= (Tests.nestedOrdering)
    //     .+= (Tests.chainOrderings(Tests.nestedOrdering,Tests.complete))
    //     .+= (Tests.barycentric)
    //     .+= (Tests.chainOrderings(Tests.barycentric,Tests.complete))
    //     .+= (Tests.bidirectional)
    //     .+= (Tests.chainOrderings(Tests.bidirectional,Tests.complete))
    //     .+= (Tests.tspBin)
    //     .+= (Tests.chainOrderings(Tests.tspBin,Tests.complete))
    //     .++= (List(
    //         Kernels.gaussianBlurSmooth(25,25)
    //     ).flatMap{ k =>
    //       List(ErrorType.Abs).map{e =>
    //            new ConvolutionSettings(kernel=k, errorType=e,padding=Padding.ZerosAndOnes)
    //       }
    //     })




     /*   Run all configs     */
     testingConfigs.par.map{t => new TestingDatasets(t)}.foreach(_.run)
}