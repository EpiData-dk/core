#!/bin/bash

## Prebuild script for Core:
# Extracts version and Revision information and writes to versionfile.

# Revision no:
REVNO=`svn info | grep "Last Changed Rev:" | cut -f 4 -d " "`
echo "const RevisionStr = '$REVNO';" > "epidatacore.revision.inc"


# Version no:
LPIFILE="epidatacore.lpk"
V1=`cat $LPIFILE | grep "<Version Major" | awk 'match($0,/Major="[0-9]*"/){print substr($0,RSTART+7,RLENGTH-8)}'`
if [ -z $V1 ]
then
  V1="0"
fi

V2=`cat $LPIFILE | grep "<Version Major" | awk 'match($0,/Minor="[0-9]*"/){print substr($0,RSTART+7,RLENGTH-8)}'`
if [ -z $V2 ]
then
  V2="0"
fi

V3=`cat $LPIFILE | grep "<Version Major" | awk 'match($0,/Release="[0-9]*"/){print substr($0,RSTART+9,RLENGTH-10)}'`
if [ -z $V3 ]
then
  V3="0"
fi

V4=`cat $LPIFILE | grep "<Version Major" | awk 'match($0,/Build="[0-9]*"/){print substr($0,RSTART+7,RLENGTH-8)}'`
if [ -z $V4 ]
then
  V4="0"
fi

VERSIONFILE="epidatacore.version.inc"
echo "    VersionNo: $V1;" > $VERSIONFILE
echo "    MajorRev:  $V2;" >> $VERSIONFILE
echo "    MinorRev:  $V3;" >> $VERSIONFILE
echo "    BuildNo:   $V4;" >> $VERSIONFILE
