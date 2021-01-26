ECHO Begin Mapping Drive for AutoCAD Standards

REM Remove any mapping for M Drive

NET USE M: /d /y

REM Checking for location of M drive

IF NOT EXIST "M:\" (GOTO :MAP_DFS) ELSE (GOTO :Civil_Custom)

REM Mapping the M Drive from the DFS location

:MAP_DFS
IF EXIST \\coacd.org\dfs\GIS-CAD\[CAD_Resources] (NET USE M: \\coacd.org\dfs\GIS-CAD\[CAD_Resources]) ELSE (GOTO :Civil_Custom)
(GOTO :Civil_Custom)

REM Checking for local Civil 3D Custom Data

:Civil_Custom

IF NOT EXIST "C:\Autodesk\Civil_3D\2020" (GOTO :Civil_Local) ELSE (GOTO :Civil_Stock)


REM Copying local Civil 3D Custom Data

:Civil_Local

REM Cleaning up 2020 Network Profile

RMDIR /s /q "%USERPROFILE%\AppData\Roaming\Autodesk\AutoCAD 2020"
RMDIR /s /q "%USERPROFILE%\AppData\Local\Autodesk\AutoCAD 2020"
RMDIR /s /q "%USERPROFILE%\AppData\Roaming\Autodesk\C3D 2020"
RMDIR /s /q "%USERPROFILE%\AppData\Local\Autodesk\C3D 2020"
REG DELETE "HKEY_CURRENT_USER\Software\Autodesk\AutoCAD\R23.1" /f

mkdir "C:\Autodesk\Temp\"

SETLOCAL

SET _source_Common=M:\Autodesk\Civil_3D\[Common]
SET _source_C3D2020=M:\Autodesk\Civil_3D\2020

SET _dest_Common=C:\Autodesk\Civil_3D\[Common]
SET _dest_C3D2020=C:\Autodesk\Civil_3D\2020

SET _what_Common=/COPY:DAT /S /PURGE /R:5
SET _what_C3D2020=/COPY:DAT /E /R:5

ROBOCOPY %_source_Common% %_dest_Common% %_what_Common%
ROBOCOPY %_source_C3D2020% %_dest_C3D2020% %_what_C3D2020%

attrib -s -h %_dest_Common%
attrib -s -h %_dest_C3D2020%

(GOTO :Civil_Stock)

REM Check if stock Civil 3D is configured for current user

:Civil_Stock

IF NOT EXIST "%userprofile%\AppData\Roaming\Autodesk\C3D 2020\enu\Support\C3D.cuix" (GOTO :Civil_Setup) ELSE (GOTO :Refresh_Files)

REM Configuration stock Civil 3D for current user

:Civil_Setup

CALL "C:\Autodesk\Civil_3D\2020\User\C3D_2020.lnk" /b "C:\Autodesk\Civil_3D\2020\Support\openclose.scr"
REM CALL "C:\Autodesk\Civil_3D\2020\User\C3A_2020.lnk" /b "C:\Autodesk\Civil_3D\2020\Support\openclose.scr"

(GOTO :Refresh_Files)

REM Refreshing local Custom Civil 3D Data

:Refresh_Files

SET _source_Common=M:\Autodesk\Civil_3D\[Common]
SET _source_C3D2020=M:\Autodesk\Civil_3D\2020

SET _dest_Common=C:\Autodesk\Civil_3D\[Common]
SET _dest_C3D2020=C:\Autodesk\Civil_3D\2020

SET _what_Common=/COPY:DAT /S /PURGE /R:5
SET _what_C3D2020=/COPY:DAT /E /R:5

ROBOCOPY %_source_Common% %_dest_Common% %_what_Common%
ROBOCOPY %_source_C3D2020% %_dest_C3D2020% %_what_C3D2020%

attrib -s -h %_dest_Common%
attrib -s -h %_dest_C3D2020%

REM Changing Default annotation scale list

REG DELETE "HKEY_CURRENT_USER\Software\Autodesk\AutoCAD\R23.1\ACAD-2000:409\Scale List" /f

md c:\temp

@ECHO OFF
If exist "%Temp%\~import.reg" (
 Attrib -R -S -H "%Temp%\~import.reg"
 del /F /Q "%Temp%\~import.reg"
 If exist "%Temp%\~import.reg" (
  Echo Could not delete file "%Temp%\~import.reg"
  Pause
 )
)
> "%Temp%\~import.reg" ECHO Windows Registry Editor Version 5.00
>> "%Temp%\~import.reg" ECHO.
>> "%Temp%\~import.reg" ECHO [HKEY_CURRENT_USER\Software\Autodesk\AutoCAD\R23.1\ACAD-3000:409\Scale List]
>> "%Temp%\~import.reg" ECHO " 0.ScaleName"="1 MILE"
>> "%Temp%\~import.reg" ECHO " 0.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 0.ScaleDrawingUnits"="5280.00000000"
>> "%Temp%\~import.reg" ECHO " 0.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO " 1.ScaleName"="2 MILES"
>> "%Temp%\~import.reg" ECHO " 1.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 1.ScaleDrawingUnits"="10560.00000000"
>> "%Temp%\~import.reg" ECHO " 1.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO " 2.ScaleName"="1"
>> "%Temp%\~import.reg" ECHO " 2.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 2.ScaleDrawingUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 2.ScaleType"="3"
>> "%Temp%\~import.reg" ECHO " 3.ScaleName"="5"
>> "%Temp%\~import.reg" ECHO " 3.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 3.ScaleDrawingUnits"="5.00000000"
>> "%Temp%\~import.reg" ECHO " 3.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO " 4.ScaleName"="10"
>> "%Temp%\~import.reg" ECHO " 4.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 4.ScaleDrawingUnits"="10.00000000"
>> "%Temp%\~import.reg" ECHO " 4.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO " 5.ScaleName"="20"
>> "%Temp%\~import.reg" ECHO " 5.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 5.ScaleDrawingUnits"="20.00000000"
>> "%Temp%\~import.reg" ECHO " 5.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO " 6.ScaleName"="30"
>> "%Temp%\~import.reg" ECHO " 6.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 6.ScaleDrawingUnits"="30.00000000"
>> "%Temp%\~import.reg" ECHO " 6.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO " 7.ScaleName"="40"
>> "%Temp%\~import.reg" ECHO " 7.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 7.ScaleDrawingUnits"="40.00000000"
>> "%Temp%\~import.reg" ECHO " 7.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO " 8.ScaleName"="50"
>> "%Temp%\~import.reg" ECHO " 8.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 8.ScaleDrawingUnits"="50.00000000"
>> "%Temp%\~import.reg" ECHO " 8.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO " 9.ScaleName"="60"
>> "%Temp%\~import.reg" ECHO " 9.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO " 9.ScaleDrawingUnits"="60.00000000"
>> "%Temp%\~import.reg" ECHO " 9.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "10.ScaleName"="80"
>> "%Temp%\~import.reg" ECHO "10.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "10.ScaleDrawingUnits"="80.00000000"
>> "%Temp%\~import.reg" ECHO "10.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "11.ScaleName"="100"
>> "%Temp%\~import.reg" ECHO "11.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "11.ScaleDrawingUnits"="100.00000000"
>> "%Temp%\~import.reg" ECHO "11.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "12.ScaleName"="200"
>> "%Temp%\~import.reg" ECHO "12.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "12.ScaleDrawingUnits"="200.00000000"
>> "%Temp%\~import.reg" ECHO "12.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "13.ScaleName"="300"
>> "%Temp%\~import.reg" ECHO "13.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "13.ScaleDrawingUnits"="300.00000000"
>> "%Temp%\~import.reg" ECHO "13.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "14.ScaleName"="400"
>> "%Temp%\~import.reg" ECHO "14.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "14.ScaleDrawingUnits"="400.00000000"
>> "%Temp%\~import.reg" ECHO "14.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "15.ScaleName"="500"
>> "%Temp%\~import.reg" ECHO "15.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "15.ScaleDrawingUnits"="500.00000000"
>> "%Temp%\~import.reg" ECHO "15.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "16.ScaleName"="600"
>> "%Temp%\~import.reg" ECHO "16.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "16.ScaleDrawingUnits"="600.00000000"
>> "%Temp%\~import.reg" ECHO "16.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "17.ScaleName"="800"
>> "%Temp%\~import.reg" ECHO "17.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "17.ScaleDrawingUnits"="800.00000000"
>> "%Temp%\~import.reg" ECHO "17.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "18.ScaleName"="1000"
>> "%Temp%\~import.reg" ECHO "18.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "18.ScaleDrawingUnits"="1000.00000000"
>> "%Temp%\~import.reg" ECHO "18.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "19.ScaleName"="2000"
>> "%Temp%\~import.reg" ECHO "19.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "19.ScaleDrawingUnits"="2000.00000000"
>> "%Temp%\~import.reg" ECHO "19.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "20.ScaleName"="3000"
>> "%Temp%\~import.reg" ECHO "20.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "20.ScaleDrawingUnits"="3000.00000000"
>> "%Temp%\~import.reg" ECHO "20.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "21.ScaleName"="4000"
>> "%Temp%\~import.reg" ECHO "21.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "21.ScaleDrawingUnits"="4000.00000000"
>> "%Temp%\~import.reg" ECHO "21.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "22.ScaleName"="5000"
>> "%Temp%\~import.reg" ECHO "22.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "22.ScaleDrawingUnits"="5000.00000000"
>> "%Temp%\~import.reg" ECHO "22.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "23.ScaleName"="6000"
>> "%Temp%\~import.reg" ECHO "23.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "23.ScaleDrawingUnits"="6000.00000000"
>> "%Temp%\~import.reg" ECHO "23.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "24.ScaleName"="8000"
>> "%Temp%\~import.reg" ECHO "24.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "24.ScaleDrawingUnits"="8000.00000000"
>> "%Temp%\~import.reg" ECHO "24.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "25.ScaleName"="120"
>> "%Temp%\~import.reg" ECHO "25.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "25.ScaleDrawingUnits"="120.00000000"
>> "%Temp%\~import.reg" ECHO "25.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "26.ScaleName"="140"
>> "%Temp%\~import.reg" ECHO "26.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "26.ScaleDrawingUnits"="140.00000000"
>> "%Temp%\~import.reg" ECHO "26.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "27.ScaleName"="150"
>> "%Temp%\~import.reg" ECHO "27.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "27.ScaleDrawingUnits"="150.00000000"
>> "%Temp%\~import.reg" ECHO "27.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "28.ScaleName"="160"
>> "%Temp%\~import.reg" ECHO "28.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "28.ScaleDrawingUnits"="160.00000000"
>> "%Temp%\~import.reg" ECHO "28.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "29.ScaleName"="250"
>> "%Temp%\~import.reg" ECHO "29.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "29.ScaleDrawingUnits"="250.00000000"
>> "%Temp%\~import.reg" ECHO "29.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "30.ScaleName"="2"
>> "%Temp%\~import.reg" ECHO "30.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "30.ScaleDrawingUnits"="2.00000000"
>> "%Temp%\~import.reg" ECHO "30.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "31.ScaleName"="3"
>> "%Temp%\~import.reg" ECHO "31.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "31.ScaleDrawingUnits"="3.00000000"
>> "%Temp%\~import.reg" ECHO "31.ScaleType"="2"
>> "%Temp%\~import.reg" ECHO "32.ScaleName"="4"
>> "%Temp%\~import.reg" ECHO "32.ScalePaperUnits"="1.00000000"
>> "%Temp%\~import.reg" ECHO "32.ScaleDrawingUnits"="4.00000000"
>> "%Temp%\~import.reg" ECHO "32.ScaleType"="2"
START /WAIT REGEDIT /S "%Temp%\~import.reg"
DEL "%Temp%\~import.reg"

(GOTO :Start_Civil3D)


:Start_Civil3D
ie4uinit.exe -show
start "" /b "C:\Autodesk\Civil_3D\2020\User\ESD_C3D_2020_Start.lnk"
EXIT