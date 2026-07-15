@echo off

: get the repository url into gitrepurl
for /f "delims=" %%i in ('git config --get remote.origin.url') do set gitrepurl=%%i

: get the branch into gitbranch
for /f "delims=" %%i in ('git branch --show-current') do set gitbranch=%%i

: get the version into gitversion
for /f "delims=" %%i in ('git describe --always --tags') do set gitversion=%%i

: get the commit time into gitcommittime
for /f "delims=" %%i in ('git log -1 --format^=%%cd --date^=format-local:"%%Y-%%m-%%dT%%H:%%M:%%S"') do set gitcommittime=%%i

(
  echo | set /p dummyName=#define GITREPURL "%gitrepurl%"
  echo:
  echo | set /p dummyName=#define GITBRANCH "%gitbranch%"
  echo:
  echo | set /p dummyName=#define GITVERSION "%gitversion%"
  echo:
  echo | set /p dummyName=#define GITCOMMITTIME "%gitcommittime%"
  echo:
  echo | set /p dummyName=#define BUILDTIME "%date%T%TIME:~0,2%:%TIME:~3,2%:%TIME:~6,2%"
) > ..\..\version\RepoParams.h

rem Resource.rc file for Windows
set "search=$WCREV$"
set "replace=%gitversion%"
set "inputFile=..\..\version\ResourceTemplate.rc"
set "outputFile=..\..\version\Resource.rc"

(for /f "tokens=*" %%a in ('type "%inputFile%" ^| findstr /n "^"') do (
    setlocal disableDelayedExpansion
    set "line=%%a"

    setlocal enableDelayedExpansion

    set "line=!line:*:=!"

    if defined line (
        set "line=!line:%search%=%replace%!"
        echo(!line!
    ) else echo.
    endlocal
    endlocal

)) > "%outputFile%"

@echo on
