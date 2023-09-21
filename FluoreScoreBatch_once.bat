set /a finalHour = 47
set /a curHour=0
@echo off
:loop
if exist AviFileChunk%curHour%_View0.avi if exist AviFileChunk%curHour%_View1.avi goto ResultTrue

:ResultFalse
    goto Done

:ResultTrue
	start FluoreScoreBatchLoopV2.bat %curHour% %curHour%
	if %curHour% GTR %finalHour% goto Done
	set /a curHour+=1
	goto loop

:Done