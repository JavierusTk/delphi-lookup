# Contributing to delphi-lookup

Thank you for your interest in contributing to delphi-lookup!

## Getting Started

### Prerequisites

- **RAD Studio 12** (Delphi 12) or later
- **Windows 10/11** (64-bit)
- **Git** for version control

### Building from Source

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/delphi-lookup.git
   cd delphi-lookup
   ```

2. Open `delphi-lookup.groupproj` in RAD Studio

3. Build for **Win64 Release** configuration

4. Copy DLLs from `bin/` to the output directory (or they will be found automatically)

### Running Tests

```bash
# Basic functionality test
delphi-indexer.exe --help
delphi-lookup.exe --help

# Index a test folder
delphi-indexer.exe "path/to/pascal/files" --category user

# Search
delphi-lookup.exe "TStringList" -n 5
```

## How to Contribute

### Reporting Bugs

1. Check existing issues to avoid duplicates
2. Include:
   - delphi-lookup version
   - Delphi version
   - Steps to reproduce
   - Expected vs actual behavior
   - Error messages (if any)

### Suggesting Features

1. Open an issue with the `enhancement` label
2. Describe the use case
3. Explain why existing functionality doesn't meet the need

### Submitting Pull Requests

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Make your changes
4. Ensure the project compiles without errors
5. Test your changes
6. Commit with clear messages
7. Push and create a Pull Request

### Code Style

- Follow existing code patterns in the project
- Use meaningful variable and function names
- Add comments for complex logic
- Keep functions focused and reasonably sized

### Commit Messages

Use clear, descriptive commit messages:

```
Add framework detection for FMX units

- Parse uses clause for FMX.* units
- Add FMX to framework enum
- Update documentation
```

## Project Structure

```
delphi-lookup/
├── bin/                    # Runtime dependencies (DLLs)
├── DelphiAST/             # Pascal AST parser (third-party)
├── Tests/                 # Test projects
├── Tools/                 # Helper scripts
├── migrations/            # Database migration scripts
├── delphi-indexer.dpr        # Indexing tool
├── delphi-lookup.dpr         # Search tool
├── uDatabaseConnection.pas # Database abstraction
├── uDatabaseBuilder.pas   # Index building logic
├── uQueryProcessor.pas    # Search query processing
└── ...
```

## Areas for Contribution

- **Documentation improvements**
- **Additional search filters**
- **Performance optimizations**
- **Framework detection enhancements**
- **New output formats**
- **IDE integration plugins**

## Questions?

Open an issue with the `question` label and we'll be happy to help!
