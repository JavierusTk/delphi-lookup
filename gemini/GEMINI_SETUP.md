# Gemini CLI Setup for delphi-lookup

This guide helps you configure your Gemini CLI to use `delphi-lookup` globally on your machine, regardless of where you cloned this repository.

## One-Shot Configuration Prompt

Copy and paste the following prompt into your Gemini CLI session while inside this repository's root directory:

```markdown
I have downloaded the `delphi-lookup` repository and I want to use it as my primary Pascal symbol search tool.

Please perform the following configuration steps:

1.  **Locate Tools**: Determine the absolute path of the current directory to find `delphi-lookup.exe` and `delphi-indexer.exe`.
2.  **Update Global Memory**: Add a permanent entry to your `~/.gemini/GEMINI.md` (or your primary global context file) with the following specific configuration. Replace `{REPO_PATH}` with the actual absolute path you determined in step 1.

    *** Start of Configuration Block ***
    
    ## Delphi Symbol Lookup (delphi-lookup)
    
    **CRITICAL**: When asked to find Pascal symbols (functions, types, constants, classes) or resolve "Undeclared identifier" errors, **ALWAYS** use `delphi-lookup.exe` as your **FIRST** tool. Only use `grep`, `glob`, or `search_file_content` if `delphi-lookup.exe` fails or returns no relevant results.
    
    ### Tool Locations
    - **Search Tool**: `{REPO_PATH}/delphi-lookup.exe`
    - **Indexer Tool**: `{REPO_PATH}/delphi-indexer.exe`
    
    ### Usage Rules
    1.  **Symbol Definition**: To find where `MySymbol` is defined, run:
        `{REPO_PATH}/delphi-lookup.exe "MySymbol" -n 5`
    2.  **Undeclared Identifiers**: If a compiler error says "Undeclared identifier: X", run:
        `{REPO_PATH}/delphi-lookup.exe "X" -n 5`
    3.  **Concept Search**: To find code related to a concept (e.g., "JSON parsing"), run:
        `{REPO_PATH}/delphi-lookup.exe "JSON parsing" -n 5`
    4.  **Specific Categories**: Use flags like `--category stdlib` or `--category user` if the user specifies a scope.
    
    ### Maintenance
    - Remind the user to run the indexer if they add significant new code or dependencies:
      `{REPO_PATH}/delphi-indexer.exe "/path/to/project" --category user`
    
    *** End of Configuration Block ***

3.  **Confirm**: Confirm to me that you have added these rules and the correct path to your memory.
```
