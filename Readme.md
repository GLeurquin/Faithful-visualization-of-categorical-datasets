# Project Title

This project was done as a master thesis in computer science. 

Data is today collected in very large amounts from various kind of sources:
shopping, genetic tests, etc. Given the amount of data collected, one might be
interested in visualizing that data in a meaningful way.
Due to their size, producing an image of such datasets is not easy as the
image has to fit on the screen. Compression of the image is needed in order
to have fewer elements to display. However, the quality of this compression
greatly depends on the structure of the data.
Given a binary matrix represented by white and black pixels, well separated
groups of pixels (e.g. black and white points are respectively gathered together)
gives a lower loss of information because summarizing that data consisely is
easier. Moreover, reorganizing the rows and columns of a dataset can give
interesting insights in structures hidden within the data.
In this thesis, we introduce an innovative algorithm to reorder binary and
categorical matrices using convolution. The results of the algorithm can be used to display the datasets faithfully.

## Getting Started

sbt run, and choose the UI.

You can load existing datasets or load your own with the following .backup format:

The .backup files can be used as input to the algorithm.
Once a .backup file has been generated, launch the UI and select "choose backup" from the dataset dropdown.

The .backup files are generated by the function writeToFile of MyMatrixLike.scala

The format for the .backup files is the following:

* 1rst row:
text describing the dataset
* 2nd row: 
<number of rows> <number of cols> <number of categories (1 for binary, more for categorical)> <1 if colors should not be changed, 0 otherwise>
* 3rd row:
The order of the rows, space separated. There must be <number of rows> unique values from 0 to <number of rows>-1. Value v at index i means that row v (in the matrix described here after) is at index i of the matrix. In the simplest case, there is no permutation and this is a space separated list from 0 to <number of rows>-1. 
* 4th row:
The order of the columns, space separated. There must be <number of columns> unique values from 0 to <number of columns>-1. Value v at index i means that column v (in the matrix described here after) is at index i of the matrix. In the simplest case, there is no permutation and this is a space separated list from 0 to <number of columns>-1.
* 5th row:
space separated column labels. There must be <number of cols> labels. The labels correspond to the matrix described here after (not the permuted matrix).
* 6th row:
space separated row labels. There must be <number of rows> labels. The labels correspond to the matrix described here after (not the permuted matrix).
* <number of rows> following rows:
each row is a sparse representation of that row. This is a space separated list of the column indices that have value 1. This represents the first binary matrix. The last row is 5 dashes (-----). There must be <number of columns> values.
* repeat the above section <number of categories> times. Each matrix represents one value of the attributes, one hot encoded.

### Prerequisites

Scala 2.11.7 and SBT



## Running the tests

Simply run sbt test


## Built With

* [SBT](http://www.scala-sbt.org/) - Dependency Management


## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Guillaume Leurquin** - *Student*
* **Thomas Bollen** - *Student*
* **Siegfried Nijssen**  - *Supervisor*

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
