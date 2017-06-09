package thesis.utils

/**
 * Code translated from the python code available on https://github.com/dmishin/tsp-solver
 * under the Free Domain license
 */

import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable.ArrayBuffer

object TSPSolver{

    /**
    *   @return a TSP path
    */
    def solve_tsp(distances: DenseMatrix[Double], optim_steps: Int = 3, pairs_by_dist : (Int, DenseMatrix[Double]) => ArrayBuffer[(Int, Int)] = pairs_by_dist ): Array[Int] = {
        val n = distances.rows
        if(n == 0) return Array()
        if(n == 1) return Array(0)

        (0 until distances.rows).foreach{ r =>
            if(distances(r,::).t.size != n) throw new IllegalArgumentException("Matrix is not square")
        }

        val node_valency = ArrayBuffer.fill(n)(2)
        val connections = ArrayBuffer.fill(n)(ArrayBuffer[Int]())

        /**
        * Does a greedy search by joining the nearest segments at every step
        */
        def join_segments(sorted_pairs: ArrayBuffer[(Int, Int)]) = {
            val segments = ArrayBuffer.tabulate(n){ i => ArrayBuffer[Int](i)}

            var count = 0

            for((i,j) <- sorted_pairs; if(!(node_valency(i) == 0 || node_valency(j) == 0 || (segments(i) == segments(j))) && count <= n-1)){
                count += 1
                node_valency(i) -= 1
                node_valency(j) -= 1

                connections(i) += j
                connections(j) += i

                var seg_i = segments(i)
                var seg_j = segments(j)

                if(seg_j.size > seg_i.size){
                    val temp = seg_i
                    seg_i = seg_j
                    seg_j = temp
                }

                for(node_idx <- seg_j){
                    segments(node_idx) = seg_i
                }

                for(x <- seg_j; if(!seg_i.contains(x))){
                    seg_i.append(x)
                }
            }
        }

        join_segments(pairs_by_dist(n, distances))
        var stop = false

        // tries to improve the solution
        for(passn <- 0 until optim_steps; if(!stop)){
            val (nopt, dtotal) = optimize_solution(distances, connections)
            if(nopt == 0) stop = true
        }


        restore_path(connections).toArray
    }

    /**
    *   @param distances distance matrix
    *   @param connections current connections between cities
    *   @return an improved TSP path if possible
    */
    def optimize_solution(distances: DenseMatrix[Double], connections: ArrayBuffer[ArrayBuffer[Int]]) = {
        val n = connections.length
        var path = restore_path(connections)
        def ds(i: Int,j: Int) = distances(path(i), path(j))

        var d_total = 0.0
        var optimizations = 0

        // tries all possible 2-opt moves
        for(a <- 0 until n-1){
            var b = a + 1
            for(c <- b+2 until n-1){
                var d = c + 1
                var delta_d = ds(a,b) + ds(c,d) - (ds(a,c) + ds(b,d))

                // if the 2-opt move would improve the solution, apply it
                if (delta_d > 0){
                    d_total += delta_d
                    optimizations += 1
                    connections(path(a)) -= path(b)
                    connections(path(a)) += path(c)
                    connections(path(b)) -= path(a)
                    connections(path(b)) += path(d)

                    connections(path(c)) -= path(d)
                    connections(path(c)) += path(a)
                    connections(path(d)) -= path(c)
                    connections(path(d)) += path(b)
                    path = restore_path(connections)
                }
            }
        }

        (optimizations, d_total)
    }

    /**
    *   @param connections The connections between cities
    *   @return a TSP path based on the connections
    */
    def restore_path(connections: ArrayBuffer[ArrayBuffer[Int]]) = {
        val indices = connections.zipWithIndex.filter{case(conn, idx) => conn.size == 1}.map{x => x._2}
        assert(indices.size==2, "problem")
        val start = Math.min(indices(0),indices(1))

        var path = ArrayBuffer(start)
        var prev_point = -1
        var cur_point = start

        var stop = false

        while(!stop){
            var next_points = connections(cur_point).filter{pnt => pnt != prev_point}
            if(next_points.size == 0) stop = true
            else{
                val next_point = next_points(0)
                path = path :+ next_point
                prev_point = cur_point
                cur_point = next_point
            }
        }
        path
    }

    /**
    *   @param n number of cities
    *   @param distances distances between the cities
    *   @return a list of the pairs of cities ordered by distances
    */
    def pairs_by_dist(n: Int, distances: DenseMatrix[Double]) = {
        val indices = ArrayBuffer.fill(n * (n - 1) / 2)((0,0))
        var idx = 0

        for(i <- 0 until n; j <- i + 1 until n){
            indices(idx) = (i,j)
            idx +=1
        }

        indices.sortBy{ij => distances(ij._1, ij._2)}

    }
}
