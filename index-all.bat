@echo off
setlocal EnableDelayedExpansion

REM ============================================================================
REM delphi-lookup - Full Index Script
REM Creates/rebuilds delphi_symbols.db with all source folders + CHM docs.
REM
REM Changes vs previous version (see W:\bin\INDEX-ALL-FIXES.md):
REM   - Corrected path typos that silently dropped 5 of 26 declared folders
REM   - Added --scan-packages step to fill the framework-detection package tier
REM   - Added --index-chm loop to populate the official_help category
REM   - Added pre-flight "if exist" check that prints [SKIP] instead of silently
REM     wasting indexer-time on a non-existent path
REM
REM Usage: index-all.bat [--force]
REM   --force  Full reindex (repopulates all data including is_declaration)
REM   default  Incremental (only new/modified files)
REM ============================================================================

set INDEXER=delphi-indexer.exe
set FORCE_FLAG=%1
set START_TIME=%TIME%

REM Quote the entire RHS so cmd doesn't break on the (x86) parentheses inside
REM if/for blocks downstream.
set "DELPHI_SRC=C:\Program Files (x86)\Embarcadero\Studio\23.0\source"
set "DELPHI_LIB=C:\Program Files (x86)\Embarcadero\Studio\23.0\lib"
set "DELPHI_HELP=C:\Program Files (x86)\Embarcadero\Studio\23.0\Help\Doc"

echo.
echo ============================================================================
echo delphi-lookup Full Indexing %FORCE_FLAG%
echo Started: %DATE% %TIME%
echo ============================================================================
echo.

REM ============================================================================
REM STEP 0: SCAN PACKAGES (fills framework-detection package tier)
REM ============================================================================
echo.
echo [PACKAGES] Scanning .dpk packages for framework detection...
echo ----------------------------------------------------------------------------

call :scan_packages "%DELPHI_LIB%" stdlib
call :scan_packages "W:\Packages290" user
call :scan_packages "W:\VCL" user

REM ============================================================================
REM STEP 1: STANDARD LIBRARY (Delphi 12 source)
REM ============================================================================
echo.
echo [STDLIB] Indexing Delphi 12 Standard Library...
echo ----------------------------------------------------------------------------

call :index_folder "%DELPHI_SRC%\rtl"  stdlib RTL
call :index_folder "%DELPHI_SRC%\vcl"  stdlib VCL
call :index_folder "%DELPHI_SRC%\fmx"  stdlib FMX
call :index_folder "%DELPHI_SRC%\data" stdlib

REM ============================================================================
REM STEP 2: USER LIBRARIES (Custom frameworks and modules)
REM ============================================================================
echo.
echo [USER] Indexing User Libraries...
echo ----------------------------------------------------------------------------

call :index_folder "W:\VCL"          user
call :index_folder "W:\CyberMAX"     user
call :index_folder "W:\Almacen"      user
call :index_folder "W:\TCConta"      user
call :index_folder "W:\Gestion2000"  user
REM La carpeta real en disco lleva tilde en la "o" (Produccion -> Producci*n).
REM No la escribimos literal: cmd lee el .bat en codepage OEM y este fichero es
REM UTF-8, asi que una ruta acentuada literal no resolveria. La expandimos por
REM comodin (? = 1 caracter). El comodin amplio "Produc*" NO vale: capturaria
REM tambien W:\Produccion-CLAUDE-md.
set "_PROD="
for /d %%D in ("W:\Producci?n") do set "_PROD=%%~fD"
if defined _PROD (
    call :index_folder "!_PROD!" user
) else (
    echo [SKIP] not found: W:\Producci?n
)
call :index_folder "W:\Clientes"     user
call :index_folder "W:\Proyectos"    user
call :index_folder "W:\EDI"          user
call :index_folder "W:\Prestashop"   user
call :index_folder "W:\Norma43"      user

REM ============================================================================
REM STEP 3: THIRD-PARTY LIBRARIES
REM ============================================================================
echo.
echo [THIRD_PARTY] Indexing Third-Party Libraries...
echo ----------------------------------------------------------------------------

call :index_folder "W:\mORMot2"                 third_party
call :index_folder "W:\HCL"                     third_party
call :index_folder "W:\OXml"                    third_party
call :index_folder "W:\DragDropSuite"           third_party
call :index_folder "W:\DropMaster"              third_party
call :index_folder "W:\UniDAC"                  third_party
call :index_folder "W:\ZipForge"                third_party
call :index_folder "W:\OExport"                 third_party
REM Re-add these if/when they reappear on this machine:
REM call :index_folder "W:\ElevateDB 2 VCL-CS-SRC" third_party
REM call :index_folder "W:\RBE"                    third_party
REM call :index_folder "W:\SecureBridge"           third_party

REM ============================================================================
REM STEP 4: OFFICIAL CHM DOCUMENTATION (populates source_category=official_help)
REM ============================================================================
echo.
echo [OFFICIAL_HELP] Indexing CHM documentation...
echo ----------------------------------------------------------------------------

if exist "%DELPHI_HELP%" (
    for %%C in ("!DELPHI_HELP!\*.chm") do (
        echo.
        echo Indexing %%~nxC...
        %INDEXER% --index-chm "%%C" --delphi-version 12.0 %FORCE_FLAG%
        if errorlevel 1 (
            echo WARNING: Error indexing %%~nxC, continuing...
        )
    )
) else (
    echo [SKIP] Help directory not found: !DELPHI_HELP!
)

REM ============================================================================
REM SUMMARY
REM ============================================================================
echo.
echo ============================================================================
echo Indexing Complete!
echo Started:  %START_TIME%
echo Finished: %TIME%
echo ============================================================================
echo.

if exist delphi_symbols.db (
    echo Database: delphi_symbols.db
    for %%F in (delphi_symbols.db) do echo Size: %%~zF bytes
)

echo.
echo Run 'delphi-lookup.exe "query"' to search the index.
echo.

goto :end

REM ============================================================================
REM Subroutine :index_folder <path> <category> [framework]
REM Pre-flight existence check then invoke the indexer. Uses delayed expansion
REM (!var!) inside if/for blocks because some paths contain parens — e.g.
REM "C:\Program Files (x86)\..." — which break parse-time %var% inside blocks.
REM ============================================================================
:index_folder
set "_PATH=%~1"
set "_CAT=%~2"
set "_FW=%~3"
if not exist "%_PATH%\" (
    echo [SKIP] not found: !_PATH!
    exit /b 0
)
echo.
echo Indexing !_PATH! [category=!_CAT!]...
if defined _FW (
    %INDEXER% "!_PATH!" --category !_CAT! --framework !_FW! %FORCE_FLAG%
) else (
    %INDEXER% "!_PATH!" --category !_CAT! %FORCE_FLAG%
)
if errorlevel 1 (
    echo WARNING: Error indexing !_PATH!, continuing...
)
exit /b 0

REM ============================================================================
REM Subroutine :scan_packages <path> <category>
REM Scan a folder for .dpk packages so framework detection can use them.
REM ============================================================================
:scan_packages
set "_PATH=%~1"
set "_CAT=%~2"
if not exist "%_PATH%\" (
    echo [SKIP] not found: !_PATH!
    exit /b 0
)
echo.
echo Scanning packages in !_PATH! [category=!_CAT!]...
%INDEXER% --scan-packages "!_PATH!" --category !_CAT!
if errorlevel 1 (
    echo WARNING: Error scanning packages in !_PATH!, continuing...
)
exit /b 0

:error
echo.
echo ============================================================================
echo INDEXING FAILED
echo ============================================================================
exit /b 1

:end
endlocal
