@echo off
REM ========================================
REM delphi-lookup Batch Indexing Script
REM ========================================
REM
REM Copy this file to delphi-scan.bat and customize paths for your environment.
REM
REM Environment variables (optional):
REM   OLLAMA_URL    - Ollama server URL (default: http://127.0.0.1:11434)
REM   RERANKER_URL  - Reranker service URL (default: http://127.0.0.1:8501)
REM
REM ========================================

SET INDEXER=%~dp0delphi-indexer.exe

REM ========================================
REM User Code (your project source folders)
REM ========================================
REM Customize these paths for your project:

REM %INDEXER% "C:\Projects\MyProject\src" --category user
REM %INDEXER% "C:\Projects\MyFramework" --category user

REM ========================================
REM Delphi Standard Library
REM ========================================
REM Adjust the Studio version path (23.0 = Delphi 12.0):

SET DELPHI_SOURCE=C:\Program Files (x86)\Embarcadero\Studio\23.0\source

%INDEXER% "%DELPHI_SOURCE%\rtl" --category stdlib --framework RTL
%INDEXER% "%DELPHI_SOURCE%\vcl" --category stdlib --framework VCL
%INDEXER% "%DELPHI_SOURCE%\fmx" --category stdlib --framework FMX
%INDEXER% "%DELPHI_SOURCE%\data" --category stdlib --framework RTL

REM Additional stdlib folders (uncomment as needed):
REM %INDEXER% "%DELPHI_SOURCE%\soap" --category stdlib --framework RTL
REM %INDEXER% "%DELPHI_SOURCE%\xml" --category stdlib --framework RTL
REM %INDEXER% "%DELPHI_SOURCE%\ToolsAPI" --category stdlib --framework RTL
REM %INDEXER% "%DELPHI_SOURCE%\DUnitx" --category stdlib --framework RTL

REM ========================================
REM Third-Party Libraries
REM ========================================
REM Add your third-party library paths:

REM %INDEXER% "C:\Libraries\mORMot2\src" --category third_party --framework RTL
REM %INDEXER% "C:\Libraries\OtherLib\src" --category third_party

REM ========================================
REM Delphi Official Documentation (CHM)
REM ========================================
REM Index CHM help files for API documentation:

SET DELPHI_HELP=C:\Program Files (x86)\Embarcadero\Studio\23.0\Help\Doc

REM Core frameworks (recommended):
%INDEXER% --index-chm "%DELPHI_HELP%\vcl.chm" --delphi-version 12.0
%INDEXER% --index-chm "%DELPHI_HELP%\system.chm" --delphi-version 12.0
%INDEXER% --index-chm "%DELPHI_HELP%\data.chm" --delphi-version 12.0

REM Additional documentation (uncomment as needed):
REM %INDEXER% --index-chm "%DELPHI_HELP%\fmx.chm" --delphi-version 12.0
REM %INDEXER% --index-chm "%DELPHI_HELP%\libraries.chm" --delphi-version 12.0
REM %INDEXER% --index-chm "%DELPHI_HELP%\topics.chm" --delphi-version 12.0

echo.
echo Indexing complete!
echo Run "delphi-indexer.exe --list-folders" to see indexed content.
pause
