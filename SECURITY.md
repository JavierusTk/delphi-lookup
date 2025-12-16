# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 1.x     | :white_check_mark: |

## Reporting a Vulnerability

If you discover a security vulnerability in delphi-lookup, please report it responsibly:

1. **Do not** open a public GitHub issue for security vulnerabilities
2. Send a detailed report to the maintainers privately
3. Include:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Suggested fix (if any)

## Security Considerations

### Database Security

- delphi-lookup creates a local SQLite database (`delphi_symbols.db`)
- The database contains indexed source code metadata
- No authentication is implemented - the database is accessible to anyone with file system access
- **Recommendation:** Do not index sensitive or proprietary code if the database will be shared

### Network Services

delphi-lookup connects to external services for optional features:

| Service | Default URL | Purpose | Data Sent |
|---------|-------------|---------|-----------|
| Ollama | `http://127.0.0.1:11434` | Embedding generation | Source code text |
| Reranker | `http://127.0.0.1:8501` | Search result reranking | Query + code snippets |

**Security recommendations:**
- Run Ollama locally to keep source code on your machine
- Use environment variables (`OLLAMA_URL`, `RERANKER_URL`) to configure endpoints
- Be aware that code content is sent to these services for processing

### File System Access

- delphi-indexer reads `.pas` files from specified directories
- delphi-lookup only reads from the local database
- No files are modified or deleted by these tools

### Dependencies

- `sqlite3.dll` - Official SQLite build with FTS5 support
- `vec0.dll` - sqlite-vec extension for vector search

Both DLLs are included in the `bin/` directory. For maximum security, you can:
1. Download fresh copies from official sources
2. Verify checksums before use
3. See `bin/README.md` for download instructions

## Best Practices

1. **Local execution:** Run Ollama locally rather than using remote servers
2. **Network isolation:** If indexing sensitive code, ensure network services are on localhost
3. **Database protection:** Protect `delphi_symbols.db` with appropriate file permissions
4. **Review indexed content:** The database contains source code extracts - treat it accordingly
