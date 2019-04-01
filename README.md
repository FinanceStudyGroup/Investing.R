# Investing.R
This is custom function and script for converting csv data from Investing.com to the xts format in R.

-----------------------------------------------------------------------------------------------------------------
Having collected our price- or yield data from Investing.com, the next step in the workflow might be to convert these
to a format more amenable to deeper analysis. Specifically, within R using the quantmod package we have access to
a wide range of functions and analytical methods that have been built by the R community.

While analysis could also be performed in Python using the [pandas](https://pandas.pydata.org/) package, R may be unique in its simplicity
and intuitive syntax. So this function and script derive from a personal affinity for R and its methods.

-----------------------------------------------------------------------------------------------------------------


To run this program, be sure to download the most recent version of [quantmod](http://www.quantmod.com/).

The program itself was built using [R](https://cran.r-project.org/) version 3.3.3.

To get quantmod, you might run,

```
install.packages("quantmod")
```

As for the use of the program, the entire script can be run by quadruple clicking the text and running it within R,
or the specific workflows the program automates can be run individually.

For example, in running the fourth function-folded section, we can convert all available index data downloaded using,
Investing.py. However, before running this script, it's critical to update line 24 of the code.

Here you would need to update the script for the location of your Investing.py downloads.

In opening this script, please note the novel syntax R uses for file paths, as in updating line 24,
you'll find that it isn't a simple copy and paste of the file path, but a novel file path syntax unique to R.

Note: A small LinkedIn article was made discussing the use of the function employed in this script. If additional guidance
might be necessary, the function is discussed further [here](https://www.linkedin.com/pulse/converting-csv-data-xts-use-r-franklin-monzon/).
