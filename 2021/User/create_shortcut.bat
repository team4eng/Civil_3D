@echo off

set SCRIPT="%TEMP%\%RANDOM%-%RANDOM%-%RANDOM%-%RANDOM%.vbs"

echo Set oWS = WScript.CreateObject("WScript.Shell") >> %SCRIPT%
echo sLinkFile = "%USERPROFILE%\Desktop\T4_C3D_2021.lnk" >> %SCRIPT%
echo Set oLink = oWS.CreateShortcut(sLinkFile) >> %SCRIPT%
echo oLink.TargetPath = "C:\Autodesk\Civil_3D\2021\User\2021_Set_COA_C3D.bat" >> %SCRIPT%

echo oLink.WorkingDirectory = "C:\Program Files\Autodesk\AutoCAD 2021\UserDataCache\" >> %SCRIPT%

echo oLink.IconLocation = "%SystemDrive%\Autodesk\Civil_3D\2021\User\COA_C3D.ico" >> %SCRIPT%

echo oLink.Save >> %SCRIPT%

cscript /nologo %SCRIPT%
del %SCRIPT%

