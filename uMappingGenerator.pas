unit uMappingGenerator;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  uFrameworkDetector;

type
  /// <summary>
  /// Mapping file entry
  /// </summary>
  TMappingEntry = record
    FilePath: string;
    Framework: string;
    DetectionMethod: string; // 'comment_tag', 'package_db', 'uses_analysis'
    Reason: string;
    IsPattern: Boolean; // True if this is a glob pattern, False if exact path
  end;

  TMappingEntryList = TList<TMappingEntry>;

  /// <summary>
  /// Generates framework mapping file (JSON) by scanning folders
  /// </summary>
  TMappingGenerator = class
  private
    FDetector: TFrameworkDetector;
    FMappingFile: string;
    FForceRescan: Boolean;
    FDryRun: Boolean;
    FExistingMappings: TMappingEntryList;
    FNewMappings: TMappingEntryList;
    FOrphanedFiles: TStringList;

    /// <summary>Load existing mapping file</summary>
    procedure LoadExistingMappings;

    /// <summary>Check if file is already in mapping</summary>
    function IsInMapping(const AFilePath: string): Boolean;

    /// <summary>Add new mapping entry</summary>
    procedure AddMapping(const AFilePath, AFramework, ADetectionMethod, AReason: string;
      AIsPattern: Boolean = False);

    /// <summary>Save mappings to JSON file</summary>
    procedure SaveMappings;

    /// <summary>Normalize path for storage (Windows format)</summary>
    function NormalizePathForStorage(const APath: string): string;

  public
    constructor Create(const ADatabaseFile: string; const AMappingFile: string);
    destructor Destroy; override;

    /// <summary>Scan folder and generate/update mapping file</summary>
    procedure ScanFolder(const AFolderPath: string; AForceRescan: Boolean = False;
      ADryRun: Boolean = False);

    /// <summary>List orphaned files (not in any package)</summary>
    procedure ListOrphanedFiles;

    /// <summary>Add bulk glob pattern mapping</summary>
    procedure AddPatternMapping(const APattern, AFramework, AReason: string);
  end;

implementation

uses
  System.StrUtils;

{ TMappingGenerator }

constructor TMappingGenerator.Create(const ADatabaseFile: string; const AMappingFile: string);
begin
  inherited Create;

  FMappingFile := AMappingFile;
  FDetector := TFrameworkDetector.Create(ADatabaseFile, AMappingFile);
  FExistingMappings := TMappingEntryList.Create;
  FNewMappings := TMappingEntryList.Create;
  FOrphanedFiles := TStringList.Create;

  // Load existing mappings if file exists
  if FileExists(FMappingFile) then
    LoadExistingMappings;
end;

destructor TMappingGenerator.Destroy;
begin
  FOrphanedFiles.Free;
  FNewMappings.Free;
  FExistingMappings.Free;
  FDetector.Free;
  inherited;
end;

procedure TMappingGenerator.LoadExistingMappings;
var
  JsonText: string;
  JsonObj: TJSONObject;
  Overrides: TJSONArray;
  I: Integer;
  Override: TJSONObject;
  Entry: TMappingEntry;
begin
  try
    JsonText := TFile.ReadAllText(FMappingFile, TEncoding.UTF8);
    JsonObj := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
    try
      if not Assigned(JsonObj) then
        Exit;

      Overrides := JsonObj.GetValue<TJSONArray>('overrides');
      if not Assigned(Overrides) then
        Exit;

      for I := 0 to Overrides.Count - 1 do
      begin
        Override := Overrides.Items[I] as TJSONObject;

        Entry.Framework := Override.GetValue<string>('framework');
        Entry.Reason := Override.GetValue<string>('reason');

        // Check if exact path or pattern
        if Override.TryGetValue<string>('file', Entry.FilePath) then
        begin
          Entry.IsPattern := False;
          Entry.DetectionMethod := 'existing_mapping';
          FExistingMappings.Add(Entry);
        end
        else if Override.TryGetValue<string>('pattern', Entry.FilePath) then
        begin
          Entry.IsPattern := True;
          Entry.DetectionMethod := 'existing_pattern';
          FExistingMappings.Add(Entry);
        end;
      end;

    finally
      JsonObj.Free;
    end;

  except
    on E: Exception do
      WriteLn(Format('Warning: Failed to load existing mappings: %s', [E.Message]));
  end;
end;

function TMappingGenerator.IsInMapping(const AFilePath: string): Boolean;
var
  Entry: TMappingEntry;
  NormalizedPath: string;
begin
  Result := False;
  NormalizedPath := LowerCase(NormalizePathForStorage(AFilePath));

  for Entry in FExistingMappings do
  begin
    if not Entry.IsPattern then
    begin
      if SameText(NormalizedPath, LowerCase(Entry.FilePath)) then
        Exit(True);
    end
    else
    begin
      // Check glob pattern match
      if FDetector.MatchesGlobPattern(AFilePath, Entry.FilePath) then
        Exit(True);
    end;
  end;

  // Check new mappings too
  for Entry in FNewMappings do
  begin
    if SameText(NormalizedPath, LowerCase(Entry.FilePath)) then
      Exit(True);
  end;
end;

function TMappingGenerator.NormalizePathForStorage(const APath: string): string;
begin
  // Convert Unix paths to Windows paths for storage
  Result := StringReplace(APath, '/', '\', [rfReplaceAll]);

  // Normalize drive letters
  Result := StringReplace(Result, '\mnt\w\', 'W:\', [rfIgnoreCase]);
  Result := StringReplace(Result, '\mnt\c\', 'C:\', [rfIgnoreCase]);
end;

procedure TMappingGenerator.AddMapping(const AFilePath, AFramework, ADetectionMethod, AReason: string;
  AIsPattern: Boolean = False);
var
  Entry: TMappingEntry;
begin
  Entry.FilePath := NormalizePathForStorage(AFilePath);
  Entry.Framework := AFramework;
  Entry.DetectionMethod := ADetectionMethod;
  Entry.Reason := AReason;
  Entry.IsPattern := AIsPattern;

  FNewMappings.Add(Entry);
end;

procedure TMappingGenerator.ScanFolder(const AFolderPath: string;
  AForceRescan: Boolean = False; ADryRun: Boolean = False);
var
  PasFiles: TArray<string>;
  PasFile: string;
  Count, Added, Skipped, Orphaned: Integer;
  Framework, DetectionMethod, Reason: string;
  CommentTag, PackageResult, UsesResult: string;
begin
  FForceRescan := AForceRescan;
  FDryRun := ADryRun;

  WriteLn(Format('Scanning folder: %s', [AFolderPath]));
  WriteLn(Format('Force rescan: %s', [BoolToStr(AForceRescan, True)]));
  WriteLn(Format('Dry run: %s', [BoolToStr(ADryRun, True)]));
  WriteLn;

  // Find all .pas files recursively
  PasFiles := TDirectory.GetFiles(AFolderPath, '*.pas', TSearchOption.soAllDirectories);

  WriteLn(Format('Found %d .pas files', [Length(PasFiles)]));
  WriteLn;

  Count := 0;
  Added := 0;
  Skipped := 0;
  Orphaned := 0;

  for PasFile in PasFiles do
  begin
    Inc(Count);

    // Skip if already in mapping (unless force)
    if not FForceRescan and IsInMapping(PasFile) then
    begin
      Inc(Skipped);
      Continue;
    end;

    // Detect framework using multi-tier detector
    Framework := FDetector.DetectFramework(PasFile, False); // Don't validate for warnings here

    // Determine detection method and reason
    CommentTag := FDetector.CheckCommentTag(PasFile);
    PackageResult := FDetector.CheckPackageDB(PasFile);
    UsesResult := FDetector.AnalyzeUsesClause(PasFile);

    if CommentTag <> '' then
    begin
      DetectionMethod := 'comment_tag';
      Reason := Format('Explicitly tagged as %s in source', [Framework]);
    end
    else if PackageResult <> '' then
    begin
      DetectionMethod := 'package_db';
      Reason := Format('Package-level framework: %s', [Framework]);
    end
    else if UsesResult <> '' then
    begin
      DetectionMethod := 'uses_analysis';
      Reason := Format('Detected from uses clause: %s', [Framework]);

      // This is an orphaned file (not in any package)
      Inc(Orphaned);
      FOrphanedFiles.Add(PasFile);

      // Warn and skip (user must manually add to mapping)
      WriteLn(Format('[WARNING] Orphaned file (not in any package): %s', [PasFile]));
      WriteLn(Format('          Framework detected: %s (uses clause)', [Framework]));
      WriteLn('          Please add to mapping file manually or include in a package');
      WriteLn;

      Continue; // Skip orphaned files
    end
    else
    begin
      DetectionMethod := 'folder_path';
      Reason := 'Detected from folder path (fallback)';

      if Framework = '' then
      begin
        WriteLn(Format('[WARNING] Unknown framework: %s', [PasFile]));
        WriteLn('          Manual categorization required');
        WriteLn;
        Continue; // Skip unknown files
      end;
    end;

    // Add to new mappings
    AddMapping(PasFile, Framework, DetectionMethod, Reason);
    Inc(Added);

    if Count mod 100 = 0 then
      WriteLn(Format('Processed: %d / %d files', [Count, Length(PasFiles)]));
  end;

  WriteLn;
  WriteLn('=== Scan Complete ===');
  WriteLn(Format('Total files: %d', [Length(PasFiles)]));
  WriteLn(Format('Added to mapping: %d', [Added]));
  WriteLn(Format('Skipped (already mapped): %d', [Skipped]));
  WriteLn(Format('Orphaned files (not in package): %d', [Orphaned]));
  WriteLn;

  if FOrphanedFiles.Count > 0 then
  begin
    WriteLn('WARNING: Found orphaned files not in any package.');
    WriteLn('These files must be manually added to the mapping file or included in a package.');
    WriteLn('Use ListOrphanedFiles() to see the list.');
    WriteLn;
  end;

  // Save mappings (unless dry run)
  if not FDryRun then
  begin
    SaveMappings;
    WriteLn(Format('Mapping file saved: %s', [FMappingFile]));
  end
  else
  begin
    WriteLn('[DRY RUN] Mappings not saved');
  end;
end;

procedure TMappingGenerator.SaveMappings;
var
  JsonObj: TJSONObject;
  Overrides: TJSONArray;
  Entry: TMappingEntry;
  Override: TJSONObject;
  JsonText: string;
begin
  JsonObj := TJSONObject.Create;
  try
    JsonObj.AddPair('version', '1.0');
    JsonObj.AddPair('description', 'Framework overrides for delphi-indexer');

    Overrides := TJSONArray.Create;

    // Add existing mappings first
    for Entry in FExistingMappings do
    begin
      Override := TJSONObject.Create;

      if Entry.IsPattern then
        Override.AddPair('pattern', Entry.FilePath)
      else
        Override.AddPair('file', Entry.FilePath);

      Override.AddPair('framework', Entry.Framework);
      Override.AddPair('reason', Entry.Reason);

      Overrides.AddElement(Override);
    end;

    // Add new mappings
    for Entry in FNewMappings do
    begin
      Override := TJSONObject.Create;

      if Entry.IsPattern then
        Override.AddPair('pattern', Entry.FilePath)
      else
        Override.AddPair('file', Entry.FilePath);

      Override.AddPair('framework', Entry.Framework);
      Override.AddPair('reason', Entry.Reason);
      Override.AddPair('detection_method', Entry.DetectionMethod);
      Override.AddPair('updated', FormatDateTime('yyyy-mm-dd', Now));

      Overrides.AddElement(Override);
    end;

    JsonObj.AddPair('overrides', Overrides);

    // Format JSON with indentation
    JsonText := JsonObj.Format(2);

    // Save to file
    TFile.WriteAllText(FMappingFile, JsonText, TEncoding.UTF8);

  finally
    JsonObj.Free;
  end;
end;

procedure TMappingGenerator.ListOrphanedFiles;
var
  FilePath: string;
begin
  WriteLn;
  WriteLn('=== Orphaned Files (Not in Any Package) ===');
  WriteLn;

  if FOrphanedFiles.Count = 0 then
  begin
    WriteLn('No orphaned files found.');
    Exit;
  end;

  for FilePath in FOrphanedFiles do
  begin
    WriteLn(Format('  %s', [FilePath]));
  end;

  WriteLn;
  WriteLn(Format('Total orphaned files: %d', [FOrphanedFiles.Count]));
  WriteLn;
  WriteLn('These files should either:');
  WriteLn('1. Be added to a .dpk package (recommended)');
  WriteLn('2. Be manually added to the mapping file with a comment tag');
  WriteLn;
end;

procedure TMappingGenerator.AddPatternMapping(const APattern, AFramework, AReason: string);
begin
  AddMapping(APattern, AFramework, 'manual_pattern', AReason, True);
  WriteLn(Format('Pattern mapping added: %s -> %s', [APattern, AFramework]));
end;

end.
