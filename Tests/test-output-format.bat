@echo off
REM Thin wrapper that delegates to test-output-format.ps1.
REM The .ps1 is the canonical test; this .bat is kept for muscle-memory.
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0test-output-format.ps1"
exit /b %ERRORLEVEL%
