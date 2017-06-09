package thesis.UI

import java.util.Random

import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.ValueAxis
import org.jfree.data.category.CategoryDataset
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.UnknownKeyException
import swing._

/** Class that can display a bar graph
*   
*   @param graphTitle The title of the graph
*   @param ylabel The label for the vertical axis
*   @param values The values to display on the graph
*/
class BarGraph(graphTitle:String, ylabel:String, values:Array[String]) extends Frame {
    private val dataset = new DefaultCategoryDataset()

    values.foreach{ v =>
        dataset.setValue(0.0, v, "")
    }

    /** Adds a value for the given category
    *   @param category The category on which to add value num
    *   @param num The value to add to the category
    */
    def +=(category:String, num:Double) = {
        try{
            dataset.incrementValue(num, category, "")
        }
        catch{
            case e:UnknownKeyException => dataset.setValue(num, category, "")
        }
    }

    /** Updates the value for the given category
    *   @param category The category on which to add value num
    *   @param num The value to add to the category
    */
    def update(category:String, num:Double) = {
        dataset.setValue(num, category, "")
    }

    /**
    *   @return The value for the given category
    */
    def apply(category:String) = {
        try{
            dataset.getValue(category, "")
        }
        catch{
            case e:UnknownKeyException => 0.0
        }
    }


    private val chart:JFreeChart = ChartFactory.createBarChart(
        graphTitle,
        null,
        ylabel,
        dataset,
        PlotOrientation.HORIZONTAL,
        true, // legend
        true, // tooltips
        false // urls
    )
    private val plot = chart.getPlot()
    private val label = new ChartPanel(chart)

    peer.setContentPane(label)
    peer.setLocationRelativeTo(null)

    visible = true

    size = new Dimension(500, 500)

    title = graphTitle

    override def closeOperation() {
        dispose()
        super.closeOperation()
    }

}