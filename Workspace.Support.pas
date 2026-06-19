unit Workspace.Support;

{*******************************************************************************
  Workspace.Support - CyberMAX Workspace Detection and Path Translation

  This unit provides automatic detection of CyberMAX workspaces and path
  translation for delphi-lookup results. When running from within a workspace
  directory, it translates database paths (W:\Repo\...) to workspace paths
  (W:\Repos\workspaces\<name>\Repo\...).

  Detection Logic:
  1. Check CYBERMAX_WORKSPACE environment variable (override)
  2. If not set, check if CWD contains /workspaces/<name>/
  3. Read .workspace.json for worktree_repos list
  4. Fallback: scan for directories with .git file (worktree indicator)

  Path Translation:
  - Only repos in worktree_repos are translated
  - Paths for repos not in workspace remain unchanged
*******************************************************************************}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections;

type
  TWorkspaceInfo = record
    Name: string;
    PathWindows: string;
    PathLinux: string;
    WorktreeRepos: TArray<string>;
    CyberMaxRootWindows: string;
    CyberMaxRootLinux: string;
    IsActive: Boolean;
  end;

/// <summary>
/// Detects the current workspace based on CWD or environment variables.
/// Returns a TWorkspaceInfo record with IsActive=True if workspace detected.
/// </summary>
function DetectWorkspace: TWorkspaceInfo;

/// <summary>
/// Translates a database path to workspace path if the repo is in worktree_repos.
/// Returns original path unchanged if not applicable.
/// </summary>
/// <param name="APath">Path from database (Windows format, e.g., W:\Gestion2000\...)</param>
/// <param name="AWorkspace">Workspace info from DetectWorkspace</param>
/// <returns>Translated path or original if not in workspace repos</returns>
function TranslatePathForWorkspace(const APath: string; const AWorkspace: TWorkspaceInfo): string;

implementation

const
  // Markers for workspace path detection
  WORKSPACES_PATH_UNIX = '/workspaces/';
  WORKSPACES_PATH_WIN = '\workspaces\';

  // Default CyberMAX roots (this machine uses /mnt/w/Repos/ not /mnt/w/)
  DEFAULT_ROOT_WINDOWS = 'W:\';
  DEFAULT_ROOT_LINUX = '/mnt/w/Repos/';

  // Environment variable names
  ENV_CYBERMAX_WORKSPACE = 'CYBERMAX_WORKSPACE';
  ENV_CYBERMAX_WORKTREE_REPOS = 'CYBERMAX_WORKTREE_REPOS';
  ENV_CYBERMAX_ROOT = 'CYBERMAX_ROOT';

function GetCurrentDirectory: string;
begin
  Result := TDirectory.GetCurrentDirectory;
end;

function GetEnvironmentVariable(const AName: string): string;
begin
  Result := System.SysUtils.GetEnvironmentVariable(AName);
end;

function ExtractWorkspaceNameFromPath(const APath: string): string;
var
  LowerPath: string;
  StartPos, EndPos: Integer;
  AfterWorkspaces: string;
begin
  Result := '';
  LowerPath := LowerCase(APath);

  // Try Unix-style path
  StartPos := Pos(LowerCase(WORKSPACES_PATH_UNIX), LowerPath);
  if StartPos > 0 then
  begin
    AfterWorkspaces := Copy(APath, StartPos + Length(WORKSPACES_PATH_UNIX), MaxInt);
    EndPos := Pos('/', AfterWorkspaces);
    if EndPos > 0 then
      Result := Copy(AfterWorkspaces, 1, EndPos - 1)
    else
      Result := AfterWorkspaces;
    Exit;
  end;

  // Try Windows-style path
  StartPos := Pos(LowerCase(WORKSPACES_PATH_WIN), LowerPath);
  if StartPos > 0 then
  begin
    AfterWorkspaces := Copy(APath, StartPos + Length(WORKSPACES_PATH_WIN), MaxInt);
    EndPos := Pos('\', AfterWorkspaces);
    if EndPos > 0 then
      Result := Copy(AfterWorkspaces, 1, EndPos - 1)
    else
      Result := AfterWorkspaces;
  end;
end;

function GetWorkspaceRootFromPath(const APath, AWorkspaceName: string): string;
var
  LowerPath: string;
  Marker: string;
  MarkerPos: Integer;
begin
  Result := '';
  LowerPath := LowerCase(APath);

  // Try Unix-style
  Marker := LowerCase(WORKSPACES_PATH_UNIX + AWorkspaceName);
  MarkerPos := Pos(Marker, LowerPath);
  if MarkerPos > 0 then
  begin
    Result := Copy(APath, 1, MarkerPos - 1 + Length(WORKSPACES_PATH_UNIX) + Length(AWorkspaceName));
    Exit;
  end;

  // Try Windows-style
  Marker := LowerCase(WORKSPACES_PATH_WIN + AWorkspaceName);
  MarkerPos := Pos(Marker, LowerPath);
  if MarkerPos > 0 then
    Result := Copy(APath, 1, MarkerPos - 1 + Length(WORKSPACES_PATH_WIN) + Length(AWorkspaceName));
end;

function ParseWorkspaceJson(const AJsonPath: string; var AInfo: TWorkspaceInfo): Boolean;
var
  JsonContent: string;
  JsonObj: TJSONObject;
  ReposArray: TJSONArray;
  I: Integer;
begin
  Result := False;

  if not FileExists(AJsonPath) then
    Exit;

  try
    JsonContent := TFile.ReadAllText(AJsonPath, TEncoding.UTF8);
    JsonObj := TJSONObject.ParseJSONValue(JsonContent) as TJSONObject;
    if JsonObj = nil then
      Exit;

    try
      // Read required fields
      if JsonObj.GetValue('name') <> nil then
        AInfo.Name := JsonObj.GetValue<string>('name');

      if JsonObj.GetValue('path_windows') <> nil then
        AInfo.PathWindows := JsonObj.GetValue<string>('path_windows');

      if JsonObj.GetValue('path_linux') <> nil then
        AInfo.PathLinux := JsonObj.GetValue<string>('path_linux');

      // Read worktree_repos array
      if JsonObj.GetValue('worktree_repos') <> nil then
      begin
        ReposArray := JsonObj.GetValue<TJSONArray>('worktree_repos');
        if Assigned(ReposArray) then
        begin
          SetLength(AInfo.WorktreeRepos, ReposArray.Count);
          for I := 0 to ReposArray.Count - 1 do
            AInfo.WorktreeRepos[I] := ReposArray.Items[I].Value;
        end;
      end;

      Result := AInfo.Name <> '';
    finally
      JsonObj.Free;
    end;
  except
    // Silently ignore JSON parsing errors
    Result := False;
  end;
end;

function ScanForWorktrees(const AWorkspacePath: string): TArray<string>;
var
  Dirs: TStringDynArray;
  Dir: string;
  GitFilePath: string;
  RepoList: TList<string>;
begin
  RepoList := TList<string>.Create;
  try
    // Get all subdirectories
    if TDirectory.Exists(AWorkspacePath) then
    begin
      Dirs := TDirectory.GetDirectories(AWorkspacePath);
      for Dir in Dirs do
      begin
        // A worktree has a .git file (not directory) pointing to the main repo
        GitFilePath := TPath.Combine(Dir, '.git');
        if FileExists(GitFilePath) then
          RepoList.Add(ExtractFileName(Dir));
      end;
    end;
    Result := RepoList.ToArray;
  finally
    RepoList.Free;
  end;
end;

function ParseCsvRepos(const ACsv: string): TArray<string>;
var
  Parts: TStringList;
  I: Integer;
begin
  Parts := TStringList.Create;
  try
    Parts.StrictDelimiter := True;
    Parts.Delimiter := ',';
    Parts.DelimitedText := ACsv;

    SetLength(Result, Parts.Count);
    for I := 0 to Parts.Count - 1 do
      Result[I] := Trim(Parts[I]);
  finally
    Parts.Free;
  end;
end;

function DetectWorkspace: TWorkspaceInfo;
var
  CWD: string;
  EnvWorkspace: string;
  EnvRepos: string;
  EnvRoot: string;
  JsonPath: string;
begin
  // Initialize result
  FillChar(Result, SizeOf(Result), 0);
  Result.IsActive := False;
  SetLength(Result.WorktreeRepos, 0);

  // Set default roots
  EnvRoot := GetEnvironmentVariable(ENV_CYBERMAX_ROOT);
  if EnvRoot <> '' then
  begin
    Result.CyberMaxRootWindows := EnvRoot;
    Result.CyberMaxRootLinux := EnvRoot;
  end
  else
  begin
    Result.CyberMaxRootWindows := DEFAULT_ROOT_WINDOWS;
    Result.CyberMaxRootLinux := DEFAULT_ROOT_LINUX;
  end;

  // Step 1: Check environment variable override
  EnvWorkspace := GetEnvironmentVariable(ENV_CYBERMAX_WORKSPACE);
  if EnvWorkspace <> '' then
  begin
    // Extract workspace name from env var path
    Result.Name := ExtractWorkspaceNameFromPath(EnvWorkspace);
    if Result.Name = '' then
      Result.Name := ExtractFileName(ExcludeTrailingPathDelimiter(EnvWorkspace));

    Result.PathWindows := EnvWorkspace;
    Result.PathLinux := EnvWorkspace;

    // Check for repos override
    EnvRepos := GetEnvironmentVariable(ENV_CYBERMAX_WORKTREE_REPOS);
    if EnvRepos <> '' then
      Result.WorktreeRepos := ParseCsvRepos(EnvRepos)
    else
    begin
      // Try to read .workspace.json
      JsonPath := TPath.Combine(EnvWorkspace, '.workspace.json');
      if not ParseWorkspaceJson(JsonPath, Result) then
        Result.WorktreeRepos := ScanForWorktrees(EnvWorkspace);
    end;

    Result.IsActive := True;
    Exit;
  end;

  // Step 2: Auto-detect from CWD
  CWD := GetCurrentDirectory;
  Result.Name := ExtractWorkspaceNameFromPath(CWD);

  if Result.Name = '' then
    Exit; // Not in a workspace

  // Get workspace root path
  Result.PathLinux := GetWorkspaceRootFromPath(CWD, Result.Name);
  if Result.PathLinux = '' then
    Exit;

  // Convert to Windows path for PathWindows
  // Assuming /mnt/w/Repos/workspaces/... -> W:\Repos\workspaces\...
  Result.PathWindows := StringReplace(Result.PathLinux, '/mnt/w/', 'W:\', []);
  Result.PathWindows := StringReplace(Result.PathWindows, '/', '\', [rfReplaceAll]);

  // Step 3: Check for repos override (works even without CYBERMAX_WORKSPACE)
  EnvRepos := GetEnvironmentVariable(ENV_CYBERMAX_WORKTREE_REPOS);
  if EnvRepos <> '' then
  begin
    // CSV override takes precedence over .workspace.json and scanning
    Result.WorktreeRepos := ParseCsvRepos(EnvRepos);
  end
  else
  begin
    // Try to read .workspace.json
    JsonPath := TPath.Combine(Result.PathLinux, '.workspace.json');
    if not ParseWorkspaceJson(JsonPath, Result) then
    begin
      // Fallback: scan for worktrees
      Result.WorktreeRepos := ScanForWorktrees(Result.PathLinux);
    end;
  end;

  Result.IsActive := Length(Result.WorktreeRepos) > 0;
end;

function IsRepoInWorktrees(const ARepoName: string; const AWorkspace: TWorkspaceInfo): Boolean;
var
  Repo: string;
begin
  Result := False;
  for Repo in AWorkspace.WorktreeRepos do
  begin
    if SameText(Repo, ARepoName) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function ExtractRepoNameFromPath(const APath: string): string;
var
  PathParts: TStringList;
  I: Integer;
  Part: string;
begin
  Result := '';

  // Handle Windows paths like W:\Gestion2000\... or W:\Repos\Gestion2000\...
  // The repo name is the first directory after the drive or after "Repos"

  PathParts := TStringList.Create;
  try
    // Normalize separators and split
    PathParts.StrictDelimiter := True;
    PathParts.Delimiter := '\';
    PathParts.DelimitedText := StringReplace(APath, '/', '\', [rfReplaceAll]);

    // Find the repo name
    // Paths can be: W:\RepoName\... or W:\Repos\RepoName\...
    for I := 0 to PathParts.Count - 1 do
    begin
      Part := PathParts[I];

      // Skip drive letter and common parent directories
      if (Part = '') or (Length(Part) = 2) and (Part[2] = ':') then
        Continue;

      // Skip "Repos" directory if present
      if SameText(Part, 'Repos') then
        Continue;

      // Skip "workspaces" - should not happen in DB paths but be safe
      if SameText(Part, 'workspaces') then
        Continue;

      // This should be the repo name
      Result := Part;
      Exit;
    end;
  finally
    PathParts.Free;
  end;
end;

function TranslatePathForWorkspace(const APath: string; const AWorkspace: TWorkspaceInfo): string;
var
  RepoName: string;
  PathAfterRepo: string;
  RepoPos: Integer;
begin
  Result := APath;

  // If workspace not active, return original path
  if not AWorkspace.IsActive then
    Exit;

  // Extract repo name from path
  RepoName := ExtractRepoNameFromPath(APath);
  if RepoName = '' then
    Exit;

  // Check if this repo is in the workspace's worktree list
  if not IsRepoInWorktrees(RepoName, AWorkspace) then
    Exit;

  // Find the repo name in the original path and extract everything after it
  // Path format: W:\Gestion2000\subdir\file.pas -> W:\Repos\workspaces\name\Gestion2000\subdir\file.pas
  RepoPos := Pos('\' + RepoName + '\', APath);
  if RepoPos = 0 then
  begin
    // Try without trailing slash (repo at end of path)
    RepoPos := Pos('\' + RepoName, APath);
    if (RepoPos > 0) and (RepoPos + Length('\' + RepoName) = Length(APath) + 1) then
      PathAfterRepo := ''
    else
      Exit; // Repo name not found in path
  end
  else
    PathAfterRepo := Copy(APath, RepoPos + Length('\' + RepoName), MaxInt);

  // Build the translated path
  // Use Windows-style path: W:\Repos\workspaces\<name>\<repo>\...
  Result := IncludeTrailingPathDelimiter(AWorkspace.PathWindows) + RepoName + PathAfterRepo;
end;

end.
