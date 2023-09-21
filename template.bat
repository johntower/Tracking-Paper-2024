@echo on

FluoreScoreCMDV2.exe AviFileChunk0_View0.avi 5 AviFileChunk0_View1.avi 5 n -1 AviFileChunk0_View0_mask.jpg 243 55 415 516 AviFileChunk0_View1_mask.jpg 249 39 436 538
SqueezeData_V1.1.exe AviFileChunk0_View0.csv AviFileChunk0_View1.csv AviFileChunk0.csv 4 4
