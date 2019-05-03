# STD_AT2
## Definition of Data Sources that are being utilised

<p>Copy the below as a template to enter your data into the dictionary</p>
START TEMPLATE ----------------------</br>
<p><B>Data: Enter Name</B> </br>
<i>Input File:</i> Enter filename thats in the repo </br>
<i>Source:</i> Enter URL or source API etc <br>
<i>Measure Description:</i> Enter Description <br>
<i>Notes from source:</i> Enter any explanatory notes <br>
<i>Variables in Input File:</i> Enter names of data columns in source file <br>
<i>Variables in Cleaned Data:</i> Enter names of data columns in finalised cleaned file <br>
<i>Comments:</i> Enter any comments that may be helpful to know when we use or interpret the data</p>
---------------------------------------END TEMPLATE<br/>

<p><B>Data: LGA to LHD Mapping</B> </br>
<i>Input File:</i> LGAtoLDH.xlsx </br>
<i>Source:</i> Constructed from unique variables for LHD and LGA in our other data sets, and merged with info found at health NSW sites </br> 
<i>Measure Description:</i> No measures - mapping names only <br>
<i>Notes from source:</i> https://www.health.nsw.gov.au/lhd/Documents/lhd-wall-map.pdf <br>
<i>Variables in Input File:</i>LGA; LHD <br>
<i>Variables in Cleaned Data:</i> LGA; LHD <br>
<i>Comments:</i> LGA = Local Govt Area; LHD = Local Health District.  Some LGA names have changed since the health NSW site was published, so combined with a web search to find new names to create final file.  File will be used for merging other data sets</p>

<p><B>Data: Alcohol Frequency</B> </br>
<i>Input File:</i> beh_alcfreq_lhn_trend.csv </br>
<i>Source:</i> www.healthstats.nsw.gov.au/Indicator/beh_alcfreq/beh_alcfreq_lhn_trend <br>
<i>Measure Description:</i> Alcohol drinking frequency in adults, 2002-2003 to 2016-2017 <br>
<i>Notes from source:</i> NSW Population Health Survey (SAPHaRI). Centre for Epidemiology and Evidence, NSW Ministry of Health. <br>
<i>Variables in Input File:</i> Local Health Districts; year; Sex; Drinking frequency; Number of Respondents; Actual estimate (Per cent); LL 95% CI; UL 95% CI <br>
<i>Variables in Cleaned Data:</i> Local Health Districts; year; Sex; freq_daily; freq_less_weekly; freq_never; freq_weekly <br>
<i>Comments:</i> Only available by Local Health District, not available by LGA. Rate of respondents by frequency converted into variables in columns for use in regression models. Data is based on a financial type year basis (e.g. 2002-03). </p>

<p><B>Data: Alcohol Hospitalisations </B> </br>
<i>Input File:</i> beh_alcafhos_lhn_trend.csv </br>
<i>Source:</i> http://www.healthstats.nsw.gov.au/Indicator/beh_alcafhos/beh_alcafhos_lhn_trend </br>
<i>Measure Description:</i> Alcohol attributable deaths 2001-2002 to 2015-2016 </br>
<i>Notes from source:</i> NSW Population Health Survey (SAPHaRI). Centre for Epidemiology and Evidence, NSW Ministry of Health. </br>
<i>Variables in Input File:</i> Local Health Districts; year; State comparison; Sex; Number; Rate per 100,000 population; LL 95% CI; UL 95% CI </br>
<i>Variables in Cleaned Data:</i> Local Health Districts; year; sex; hospitalisation_num; hospitalisation_rate </br>
<i>Comments:</i> Data is based on a financial type year basis (e.g. 2002-03).  Also available by LGA – chose to use the Local Health District data so it can be joined to frequency data </p>

<p><B>Data: Alcohol Deaths</B> </br>
<i>Input File:</i> beh_alcafdth_lhn_trend.csv </br>
<i>Source:</i> http://www.healthstats.nsw.gov.au/Indicator/beh_alcafdth/beh_alcafdth_lhn_trend  </br>
<i>Measure Description:</i> Alcohol attributable deaths 2001-2002 to 2015-2016  </br>
<i>Notes from Source:</i> Mortality estimates for years up to 2005 are based on Australian Bureau of Statistics death registration data. Data from 2006 onwards were provided by the Australian Coordinating Registry, Cause of Death Unit Record File; the data for the most 2 recent years are preliminary (SAPHaRI, Centre for Epidemiology and Evidence, NSW Ministry of Health)  </br>
<i>Variables in Input File:</i> Local Health Districts; year; Sex; State comparison; Average number per year; Rate per 100,000 population; LL 95% CI; UL 95% CI  </br>
<i>Variables in Cleaned Data:</i> Local Health Districts; year; sex; death_num; death_rate  </br>
<i>Comments:</i> Data is based on a financial type year basis (e.g. 2002-03).  Also available by LGA – chose to use the Local Health District data so it can be joined to frequency data  </p>

<p><B>Data: Postcode Data</B> </br>
<i>Input File:</i> 7_PostcodeData2018.csv </br>
<i>Source:</i> https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Datasets-.aspx  </br>
<i>Measure Description:</i> Monthly data on all criminal incidents recorded by NSW police Jan-1995 and Dec-2018 allocated by Postcode  </br>
<i>Notes from Source:</i> This data is sourced from BOCSAR and is updated either annually or quarterly. This is for public use so no legal ramifications associated. This data was released on 6th March 2019 so quite latest and up-to-date. </br>
<i>Variables in Input File:</i> Postcode; offence category; subcategory; month/year (time as variable)  </br>
<i>Variables in Cleaned Data:</i> Postcode; offence category; subcategory; time of year as month-year, crime volume  </br>
<i>Comments:</i>   </p>

<p><B>Data: Postcode Data</B> </br>
<i>Input File:</i> 8_RCI_offencebymonth.xlsm </br>
<i>Source:</i> https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Datasets-.aspx  </br>
<i>Measure Description:</i> Monthly data on all criminal incidents recorded by NSW police Jan-1995 and Dec-2018 allocated by Local Government Areas or LGA  </br>
<i>Notes from Source:</i> This data is sourced from BOCSAR and is updated either annually or quarterly. This is for public use so no legal ramifications associated. This data was released on 6th March 2019 so quite latest and up-to-date. </br>
<i>Variables in Input File:</i> Postcode; offence category; subcategory; month/year (time as variable)  </br>
<i>Variables in Cleaned Data:</i> Postcode; offence category; subcategory; time of year as month-year, crime volume  </br>
<i>Comments:</i> This file seems corrupted as the names of months are changed into random numbers. This will need to be sorted for it to be useful. Once this is done, it can be merged with Postcode data and LGA-Postcode alignment file from Cuong to have coherent data.   </p>
