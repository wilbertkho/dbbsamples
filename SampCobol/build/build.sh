#!/bin/sh
##############################################################################################
##
##  This sample shell script is provided as an easy way to invoke the Mortgage Application
##  sample build from the command line.
##
##  usage: build.sh [options] <buildfile>
##
##  buildFile:  Relative path (from sourceDir) of the file to build. If file
##  is *.txt then is assumed to be a buildlist file containing a list of relative
##  path files to build. The build list file can be absolute or relative (from
##  sourceDir) path.  If omitted then HealthCareApp/build/files.txt is built.
##
##  **NOTE - Assumes HealthCareApp/build/build.properties has been configured with
##           default properties.  Default properties can be overridden using command options
##           described in HealthCareApp/build/build.groovy
##
##  Examples:
##  build.sh
##  build.sh HealthCareApp/cobol/epsnbrvl.cbl
##  build.sh --propFile /u/usr/myProps.properties /u/usr/buildFiles.txt
##
###############################################################################################

# Set the DBB bin directory
DBB_HOME=/var/dbb/1.0.1
export JAVA_HOME=/usr/lpp/java/J8.0_64/

# $DBB_HOME/bin/groovyz automatically sets the env variables and classpath required for DBB
CMD="$DBB_HOME/bin/groovyz build.groovy $@"

echo $CMD
$CMD
