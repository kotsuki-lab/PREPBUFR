# 1 Contents
## 1-1 Software
<dl>
	<dt>PREPBUFR_v02/</dt>
	<dd>
Programs and scripts for decoding and observation thinning of PREPBUFR.
	</dd>
</dl>

DOI: [10.5281/zenodo.15029781](https://doi.org/10.5281/zenodo.15029781)

## 1-2 Related resource
<dl>
	<dt>PREPBUFR_v02_out_min</dt>
	<dd>
Output data of observation thinning of PREPBUFR.

They are generated through the following processes, so the user does not need to use it, but they can use it for checking their products.

DOI: [10.5281/zenodo.15029720](https://doi.org/10.5281/zenodo.15029720)
	</dd>
</dl>
<dl>
	<dt>ClimaX-LETKF_v1.13</dt>
	<dd>
ClimaX-LETKF system version 1.13.

PREPBUFR observation data are assimilated to ClimaX by LETKF.

DOI: [10.5281/zenodo.15029841](https://doi.org/10.5281/zenodo.15029841)
	</dd>
</dl>

`PBDIR` is the absolute path of this package and `CLDIR` is the absolute path of ClimaX-LETKF.

# 2 How to do observation thinning of PREPBUFR

### (1) Compile the program

#### (i) Edit Mkinclude file

Change the compiler and its options if necesarry by editting `${PBDIR}/src/Mkinclude`.

`TOPSRCDIR` indicates `${PBDIR}/src` in every Makefile.

#### (ii) Get NCEPLIBS-bufr and compile it

Download NCEPLIBS-bufr-12.1.0 and compile it following its guide.

Later version than 12.1.0 may work but not checked.

Assert that the file given by `NCEPLIB` in `${PBDIR}/src/Mkinclude` exists.

#### (iii) Compile the program

	$ cd ${PBDIR}/src
	$ make clean && make

Then, the main program is generated as `${PBDIR}/main`.

Users run this program directly not directly but through the shell scripts described later.

### (3) Download PREPBUFR data

1) Access "NCEP ADP Global Upper Air and Surface Weather Observations (PREPBUFR format)" (https://rda.ucar.edu/datasets/d337000/dataaccess/#).

2) Click "Web File Listing" at the row "UNION OF AVAILABLE PRODUCTS" and the column "DATA FILE DOWNLOADS".

3) Click "LINK" in the "Complete File List".

4) Select the data file you need and download it.


### (4) Decode PREPBUFR data
Decode downloaded BUFR format PREPBUFR data with the script `001_exec_dump.sh`.

#### Parameters
`DIR_PREPBUFR` gives the directory of input prepbufr data (both absolute and relative are allowed).

Each data is stored as:

`$DIR_PREPBUFR/${yy}/${yy}${mm}${dd}/prepbufr.gdas.${yy}${mm}${dd}.t${HH}z.nr`,

For example, the file of 18:00UTC, January 31, 2016 is stored as:

`$DIR_PREPBUFR/2016/20160131/prepbufr.gdas.20160131.t18z.nr`

#### Arguments
The first format gets two arguments, year and month as follows:

	$ ./001_dump.sh 2016 1

In this case, data from 00:00UTC, January 1, 2016 to 18:00UTC, January 31, 2016 are decoded.

The second format gets one argument of date and time in the format of yyyymmddHH as follows:

	$ ./001_dump.sh 2016010100

In this case, data on 00:00UTC, January 1, 2016 are decoded.

Decoding one data takes approximately 30 sec, so we recommend you to run this script in parallel. Running for each month using the first input format will be a good option.

### (5) Apply observation thinning
Conduct observation thinning with the script `002_exec_thinning.sh`.

#### Parameters
Important parameters are as follows.
<dl>
	<dt>RUNNAME</dt>
	<dd>

The name of the product. Users can give any name.

Observation data is output to the directory `out/thinning/${RUNNAME}/obs/`.

Output files are named as `yyyymmddhh.dat` in this directory. For example, `2016010206.dat` is the data on 06:00UTC, January 2, 2016.
	</dd>
	<dt>NMAX</dt>
	<dd>
A maximum number of the selected observation points for each category.
	</dd>
	<dt>RHO_H, RHO_V</dt>
	<dd>
Localization scales of great-circular distance (km) and natural logarithm of pressure (log hPa), respectively. 

They can take the values different from those of LETKF.
	</dd>
	<dt>DNST_THRESH</dt>
	<dd>
A threshold of observation density.
	</dd>
	<dt>WGT_QLT</dt>
	<dd>
A list of quality weights for quality markers of 0, 1, 2, 3 and others.
	</dd>
</dl>

#### Arguments
This script takes the same format of arguments as `001_exec_dump.sh`.


# 3 How to make data for drawing observation overview

Just plotting observation points as it is is not appropriate because:
* The number of observation points is too large to plot.
* The figure's appearance is affected by the order of plotting the points.

Therefore, we made the figure as follows:

1. Divide the Earth's surface into 3600x1800 grids.
2. Compute averages of coordinates and observational values of observations in each grid.
3. Plot them.

### (1) Compile the program

	$ cd ${PBDIR}/src/fig
	$ make clean && make

### (2) Check if the data exist

Decoded PREPBUFR data must be in the directory `${PBDIR}/out/dump/${year}/${datetime}/`, where `$year` is the year and `$datetime` is the date and time with the format of `yyyymmddHH` such as `2016010218`, which means 18:00 UTC, January 2, 2016.

### (3) Run the program

	$ cd ${PBDIR}/src/fig
	$ python run.py $msgtyp $time_bgn $time_end

`$msgtyp` is the category of PREPBUFR data such as "ADPUPA", "SFCSHP", and "SATWND". See Table 1.a on "https://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_1.htm".

`$time_bgn` and `$time_end` gives the initial time and the final time, respectively.

Then the data for drawing are generated in the directory `${PBDIR}/out/fig/${year}/${datetime}`.

### (4) Draw the figure

Described in the next section.


# 4 How to draw figures

This section describes how to draw the figures in Takeshima et al., 2025.

Do it after corresponding experiments have been finished.

Use the script `mkfig.py` in `${PBDIR}` for drawing figures.

It takes the name of the job for the first argument, and the arguments required for this job for the latter arguments.


#### Drawing an overview of PREPBUFR observations (Figure 2)

* Usage 

		$ python mkfig.py map_obs_overview $msgtyp $var $lev $time_bgn $time_end $is_point

* Example

		$ python mkfig.py map_obs_overview SFCSHP Ps 0 2016010100 2017123118 F

* Description

  `$msgtyp` is the category of PREPBUFR data.

  `$var` is the variable name. U, V, T, Q, G, and Ps for zonal wind, meridional wind, temperature, specific hhumidity, geopotential, and surface pressure, respectively.

  `$lev` is the model layer, which can take 1 to 7 (1 is bottom). For surface pressure, it does not affect plotting.



#### Drawing timeseries of RMSE and ensemble spread of multiple experiments (Figure 3)

* Usage

		$ python mkfig.py timeseries_stats $dir_data $var $lev

* Example

		$ python mkfig.py timeseries_stats ${CLDIR}/DATA/prepbufr T 5

* Description

  `$dir_data` is the directory of output directories of experiments, namely `${CLDIR}/DATA/XX` in "2-4 Running experiments (3) Edit main script of experiments and run it" in the readme document of ClimaX-LETKF.

  `$var` is the variable name. The format is same to the above.

  `$lev` is the model layer. The format is same to the above.

  In this directory, a directory of assimilated observations `obs` and directories of experiments' outputs such as `V250122_CLIMAXdef-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00` exist.

  This job needs experiment results of RTPS with the relaxation factors of 0.9, 1.0, 1.1, 1.2, and 1.3 and RTPP with the relaxation factors of 0.80, 0.85, 0.90, 0.95, and 1.00, that are given in the variable "lst_factor" in the function "draw_timeseries_stats".

#### Drawing maps of RMSE, ensemble spread and the number of observations (Figure 4)

* Usage
		$ python mkfig.py fields $dir_run $var $lev $time_bgn $time_end $show

* Example

		$ python mkfig.py fields ${CLDIR}/speedy-climax/DATA/${obs_name}/V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPP0.90_ad0.00_po0.00/ T 5 2016020100 2017123118 T

* Description

  `$dir_run` is the experimet results' output directory. `$obs_name` in the example is the name of processed observation data and it takes the form of `prepbufr_$RUNNAME`. `$RUNNAME` is what defined in "2 How to do observation thinning of PREPBUFR (5) Apply observation thinning".

  `$var` is the variable name. The format is same to the above.

  `$lev` is the model layer. The format is same to the above.

  `$time_bgn` and `$time_end` gives the span of the data. With this example, the figures for the data from 00:00UTC, February 1, 2016 to 18:00UTC, December 31, 2017 is drawn.

  `$show` determines if or not display the generated figure by "T" or "F" for true or false, respactively.

#### Drawing RMSE and ensemble spread against lead time (Figure 5)

* Usage

		$ python mkfig.py timeprogress_fcstexp $CLDIR $var $lev

* Example

		$ python mkfig.py timeprogress_fcstexp $CLDIR T 5

* Description

  It uses the outputs of validation of the forecast experiments. 

  Output directories are in `${CLDIR}/speedy-climax/211_val-climax/`, and directory names are like:

  `prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_SMP2017010100-2017013118_fcst_glb_SPRD.txt`

  See the README document of ClimaX-LETKF for more details.

  `$var` is the variable name. The format is same to the above.

  `$lev` is the model layer. The format is same to the above.

  This is specific for RTPS with the relaxation factor of 1.2 and RTPP with the relaxation factor of 0.90, and for January 2017 (same conditions for Figure 5).

