\name{JSON_summary}
\alias{JSON_summary}
\title{Create a summary time series of JSON files from all IoT instruments which meet a specified pattern.}
\description{JSON_summary takes a folder of files obtained from the IoT server, cleans them, summarizes them into bins of a specified averaging time interval, and saves a dataframe which includes this summarized data, some diagnostics on how the devices are running, and a record of which files have been processed and disregarded (e.g. because they did not include any data, or their structure did not fit the criteria).}
\usage{summarize_JSON<-function(JSON_folder,pattern,summary_folder,ws_name,averaging_time,timevar,idvar,datavars)}
\arguments{
  \item{JSON_folder}{The folder containing the JSON (JavaScript Object Notation) files from the IoT instruments.}
  \item{pattern}{The pattern present in the filename of all the files you want to include in the summary.}
  \item{summary_folder}{The folder where the summarized resulting R workspace should be saved.}
  \item{ws_name}{The desired name of the .Rdata workspace.}
  \item{averaging_time}{The desired averaging time like so: "02:00:00" for two hour intervals, "00:10:00" for 10-minute intervals, "00:02:00" for 2-minute intervals. Beware: specifying an averaging time smaller than the typical logging interval makes very little sense.}
  \item{timevar}{The name of the JSON variable where the date/time is stored.}
  \item{idvar}{The name of the JSON variable where the instrument ID is stored.}
  \item{datavars}{The name(s) of the variable(s) which contain the data to be averaged across the specified interval.}
}
\details{}
\value{The function returns a short statement of what was done, the number of new files found, number of new files processed, and number of files disregarded. The actual result can be found in the form of a workspace located in the summary_folder specified, under the name ws_name. The workspace includes:
  \item{big_data}{The cleaned and summarized dataset including results from all JSON files which meet the specified criteria.}
  \item{files_processed}{A vector with the names of all the files present in the JSON_folder which were already processed (so they don't need to be processed again!)}
  \item{files_disregarded}{A vector with the names of all the files which were disregarded for either of 2 reasons: (1) they did not contain any data or (2) they did not have the required file structure.}}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{Developed for Atlas Sensing Labs}
\seealso{}
\examples{
#Compile the workspace from the folder of JSON files:
summarize_JSON(JSON_folder="U:/AP_IoT/test_json/_DATA/json_data/",
               pattern="_SY_",
               summary_folder="U:/AP_IoT/test_json/",
               ws_name="SY_files",
               averaging_time="00:10:00",
               timevar="time",
               idvar="device_id",
               datavars=c("Count","RH","Temp","Ratio"))

#Load the workspace that was just created:
load("U:/AP_IoT/test_json/SY_files.Rdata")
#Generate some time series and diurnal plots:
ggplot(data=big_data[3000:4000,],aes(x=posixtime,y=Temp,group=device_id))+geom_line(aes(col=device_id))
big_data$time_hour<-hour(round(big_data$posixtime,"hour"))
ggplot(data=big_data,aes(x=as.factor(time_hour),y=Count))+geom_boxplot(aes(col=device_id))+scale_y_log10(limits=c(0.1,1000))
ggplot(data=big_data,aes(x=as.factor(time_hour),y=Temp))+geom_boxplot(aes(col=device_id))
}
\keyword{}
