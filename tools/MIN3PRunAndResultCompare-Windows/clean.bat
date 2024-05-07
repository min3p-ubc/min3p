:: batch-file to go though all the folders and run MIN3P on all *.dat files
@ echo off
FOR /R ..\Benchmarks_V2.6.Temp %%F in (.) DO (
   echo -------------------------------------------------------------------------------
   echo %%F
   cd %%F
   FOR  %%D in (*.dat) DO (
	echo %%D | del *_*.* | del *.log | del fort.* | del *.tmp1 | del *.tmp2
   )
)
cd ..\Benchmarks_V2.6.Temp
@ echo on
PAUSE
