# Script to get revision information and build information on Linux using autorevision software.

# Get revision number
VCS_NUM="$(autorevision -s VCS_NUM)"

# Get commit date
VCS_DATE=$(echo "$(autorevision -s VCS_DATE)" | cut -c1-19)
VCS_DATE2=$(echo ${VCS_DATE} | tr - / | tr T " ") 

BUILD_DATE=$(date +"%Y/%m/%d %H:%M:%S")

# Replace revision information in Version.F90 file
sed -i 's/\([[:blank:]]Character(10) :: GlobalRevisionNumber = "\)[^"]*"/\1REPLACE_VCS_NUM"/' ./Version.F90
sed -i "s?REPLACE_VCS_NUM?${VCS_NUM}?g" ./Version.F90

sed -i 's/\([[:blank:]]Character(32) :: CommitDate = "\)[^"]*"/\1REPLACE_VCS_DATE"/' ./Version.F90
sed -i "s?REPLACE_VCS_DATE?${VCS_DATE2}?g" ./Version.F90

sed -i 's/\([[:blank:]]Character(32) :: BuildDate = "\)[^"]*"/\1REPLACE_BUILD_DATE"/' ./Version.F90
sed -i "s?REPLACE_BUILD_DATE?${BUILD_DATE}?g" ./Version.F90
