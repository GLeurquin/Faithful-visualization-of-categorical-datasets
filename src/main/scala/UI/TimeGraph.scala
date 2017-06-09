package thesis.UI

import java.util.Random

import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.ValueAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.data.time.Millisecond
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.TimeSeriesCollection
import swing._

/** Class that can display a time series graph
*   
*   @param graphTitle The title of the graph
*   @param ylabel The label for the vertical axis
*   @param seriesNames The name of the series that will be displayed
*   @param maxdomain The maximum values of the series
*/
class TimeGraph(graphTitle:String, ylabel:String, seriesNames:Array[String], maxdomain:Option[Double] = None) extends Frame {
    private val ts = Array.tabulate(seriesNames.length){i => new TimeSeries(seriesNames(i))}

    /** Adds value num to series idx
    *
    *   @param idx The series index
    *   @param num The value to add
    */
    def +=(idx:Int, num:Double) = {
        ts(idx).addOrUpdate(new Millisecond(), num)
    }

    private val dataset = new TimeSeriesCollection()
    ts.foreach{s => dataset.addSeries(s)}

    private val chart:JFreeChart = ChartFactory.createTimeSeriesChart(
        graphTitle,
        "Time",
        ylabel,
        dataset,
        true, // legend
        true, // tooltips
        false // urls
    )
    private val plot:XYPlot = chart.getXYPlot()
    private val axis:ValueAxis = plot.getDomainAxis()
    axis.setAutoRange(true)
    if(maxdomain.nonEmpty) {
        axis.setFixedAutoRange(maxdomain.get)
        ts.foreach{_.setMaximumItemAge(maxdomain.get.toLong)}
    }
    private val label = new ChartPanel(chart)

    peer.setContentPane(label)
    peer.setLocationRelativeTo(null)

    visible = true
    title = graphTitle

    size = new Dimension(500, 500)

    override def closeOperation() {
        dispose()
        super.closeOperation()
    }

}