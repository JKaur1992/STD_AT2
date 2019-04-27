# STD_AT2
## Definition of Data Sources that are being utilised

Copy the below as a template to enter your data
<p><B>Data: Enter Name</B> </p>
<p><i>Input File:</i> Enter filename thats in the repo </br>
<i>Source:</i> Enter URL our source API etc <br>
<i>Measure Description:</i> Enter Description <br>
<i>Notes from source: Enter any explanatory notes <br>
<i>Variables in Input File:</i> Enter names of data columns in source file <br>
<i>Variables in Cleaned Data:</i> Enter names of data columns in finalised cleaned file <br>
<i>Comments:</i> Enter any comments that may be helpful to know when we use or interpret the data</p>

<p><B>Data: Alcohol Frequency</B> </p>
<p><i>Input File:</i> beh_alcfreq_lhn_trend.csv </br>
<i>Source:</i> www.healthstats.nsw.gov.au/Indicator/beh_alcfreq/beh_alcfreq_lhn_trend <br>
<i>Measure Description:</i> Alcohol drinking frequency in adults, 2002-2003 to 2016-2017 <br>
<i>Notes from source:</i> NSW Population Health Survey (SAPHaRI). Centre for Epidemiology and Evidence, NSW Ministry of Health. <br>
<i>Variables in Input File:</i> Local Health Districts; year; Sex; Drinking frequency; Number of Respondents; Actual estimate (Per cent); LL 95% CI; UL 95% CI <br>
<i>Variables in Cleaned Data:</i> Local Health Districts; year; Sex; freq_daily; freq_less_weekly; freq_never; freq_weekly <br>
<i>Comments:</i> Only available by Local Health District, not available by LGA. Rate of respondents by frequency converted into variables in columns for use in regression models </p>

<p><B>Data: Alcohol Hospitalisations </B> </p>
<p><i>Input File:</i> beh_alcafdth_lhn_trend.csv </br>
<i>Source:</i> http://www.healthstats.nsw.gov.au/Indicator/beh_alcafhos/beh_alcafhos_lhn_trend </br>
<i>Measure Description:</i> Alcohol attributable deaths 2001-2002 to 2015-2016 </br>
<i>Notes from source:</i> NSW Population Health Survey (SAPHaRI). Centre for Epidemiology and Evidence, NSW Ministry of Health. </br>
<i>Variables in Input File:</i> Local Health Districts; year; State comparison; Sex; Number; Rate per 100,000 population; LL 95% CI; UL 95% CI </br>
<i>Variables in Cleaned Data:</i> Local Health Districts; year; sex; hospitalisation_num; hospitalisation_rate </br>
<i>Comments:</i> Data is based on a financial type year basis (e.g. 2002-03).  Also available by LGA – chose to use the Local Health District data so it can be joined to frequency data </p>

<p><B>Data: Alcohol Deaths</B> </p>
<p><i>Input File:</i> beh_alcafdth_lhn_trend.csv </br>
<i>Source:</i> http://www.healthstats.nsw.gov.au/Indicator/beh_alcafdth/beh_alcafdth_lhn_trend  </br>
<i>Measure Description:</i> Alcohol attributable deaths 2001-2002 to 2015-2016  </br>
<i>Notes from Source:</i> Mortality estimates for years up to 2005 are based on Australian Bureau of Statistics death registration data. Data from 2006 onwards were provided by the Australian Coordinating Registry, Cause of Death Unit Record File; the data for the most 2 recent years are preliminary (SAPHaRI, Centre for Epidemiology and Evidence, NSW Ministry of Health)  </br>
<i>Variables in Input File:</i> Local Health Districts; year; Sex; State comparison; Average number per year; Rate per 100,000 population; LL 95% CI; UL 95% CI  </br>
<i>Variables in Cleaned Data:</i> Local Health Districts; year; sex; death_num; death_rate  </br>
<i>Comments:</i> Data is based on a financial type year basis (e.g. 2002-03).  Also available by LGA – chose to use the Local Health District data so it can be joined to frequency data  </p>

