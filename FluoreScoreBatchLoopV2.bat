REM This is master template for batch running of FluoreScore and related utilities.
REM copy this file to the folder that you have all video files
REM To use this file, for each experiment you need to change the input paramters for FluoreScore as well as FS3Doutput (new projection matrix if you ever change the position of cameras)
REM For a detail explanation for input parameters refer to protocol file 
REM Usage: Type in commandline: runFSOnChunks.bat firstChunk lastChunk
REM If you are not changing FS, SD, FSD and projMatFile in this file make sure you copy FluoreScoreCMDV1.exe,FluoreScoreSQV1.exe, FluoreScore3DV1.exe,projMat.txt in the video folder. 
REM Example: runFSInBatch.bat 5 10 runs the FluoreScore and SqueezeData and FS3Doutput on the chunks of 5 to 10
REM if you decide to run multiple instant of FS for one experiment,open new terminals (command prompt or cmd) and in each one call runFSInBatch.bat for different chunk numbers
REM Reza Ardekani (dehestan@usc.edu), Oct 25th 2011
@echo on
REM set /a countChunks=0
if "%1" == "" GOTO :missing
if "%2" == "" GOTO :missing

set countChunks=%1
set lastHour=%2
set /a lastHour+=1

set FS="FluoreScoreCMDV2.exe"
set SD="FluoreScoreSQV1.exe"
set FS3D="FluoreScore3DV1.exe"
set projMatFile="projMat.txt"
:startloop
if "%countChunks%"=="%lastHour%" GOTO :end
%FS% AviFileChunk%countChunks%_View0.avi 15 AviFileChunk%countChunks%_View1.avi 15 n -1 
%SD% AviFileChunk%countChunks%_View0.csv AviFileChunk%countChunks%_View1.csv AviFileChunk%countChunks%.csv AviFileChunk%countChunks%_pos1.csv AviFileChunk%countChunks%_pos2.csv 1 15
%FS3D% AviFileChunk%countChunks%_pos1.csv AviFileChunk%countChunks%_pos2.csv %projMatFile% AviFileChunk%countChunks%_3dpos.csv	
set /a countChunks+=1
GOTO :startloop
:end
:missing
echo Usage : FluoreScoreBatch.bat firstHour lastHour




