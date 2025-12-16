#!/usr/bin/env python3
"""
CHM Extraction Tool for Delphi Documentation
Extracts HTML content from Microsoft Compiled HTML Help (.chm) files
"""

import sys
import os
import json
import argparse
from pathlib import Path
from typing import Dict, List, Optional
import html
import re

# Try to import chm library
try:
    from chm import chm
    HAS_CHM_LIB = True
except ImportError:
    HAS_CHM_LIB = False
    print("Warning: pychm library not found. Install with: pip install pychm", file=sys.stderr)

class CHMExtractor:
    """Extracts content from CHM files"""

    def __init__(self, chm_path: str, output_dir: str):
        self.chm_path = Path(chm_path)
        self.output_dir = Path(output_dir)
        self.topics = []
        self.hierarchy = {}

        if not self.chm_path.exists():
            raise FileNotFoundError(f"CHM file not found: {chm_path}")

        # Create output directory structure
        self.html_dir = self.output_dir / "html"
        self.html_dir.mkdir(parents=True, exist_ok=True)

    def extract_using_library(self) -> bool:
        """Extract using pychm library (if available)"""
        if not HAS_CHM_LIB:
            return False

        try:
            print(f"Opening CHM file: {self.chm_path}")
            chm_file = chm.CHMFile()
            if not chm_file.LoadCHM(str(self.chm_path)):
                print("Failed to load CHM file")
                return False

            print("Enumerating topics...")
            topic_count = 0

            def enum_callback(chm_obj, ui, context):
                nonlocal topic_count
                path = ui.path.decode('utf-8', errors='ignore')

                # Only process HTML files
                if path.lower().endswith('.html') or path.lower().endswith('.htm'):
                    # Extract content
                    result, content = chm_file.RetrieveObject(ui)
                    if result == 1 and content:
                        # Save to output directory
                        output_path = self.html_dir / Path(path).name

                        try:
                            # Try to decode as UTF-8, fallback to latin-1
                            try:
                                text_content = content.decode('utf-8')
                            except UnicodeDecodeError:
                                text_content = content.decode('latin-1', errors='ignore')

                            output_path.write_text(text_content, encoding='utf-8')

                            # Store topic info
                            self.topics.append({
                                'path': path,
                                'file': str(output_path),
                                'size': len(content)
                            })

                            topic_count += 1
                            if topic_count % 100 == 0:
                                print(f"  Extracted {topic_count} topics...")
                        except Exception as e:
                            print(f"  Error extracting {path}: {e}", file=sys.stderr)

                return chm.CHM_ENUMERATOR_CONTINUE

            chm_file.EnumerateFiles(enum_callback, None)
            print(f"Successfully extracted {topic_count} topics")
            return True

        except Exception as e:
            print(f"Error using pychm library: {e}", file=sys.stderr)
            return False

    def extract_using_hh_exe(self) -> bool:
        """Extract using hh.exe command-line tool (Windows only)"""
        import subprocess
        import platform

        if platform.system() != 'Windows':
            print("hh.exe is only available on Windows", file=sys.stderr)
            return False

        try:
            print(f"Extracting using hh.exe...")

            # hh.exe -decompile <output_folder> <chm_file>
            cmd = ['hh.exe', '-decompile', str(self.html_dir), str(self.chm_path)]

            result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)

            if result.returncode == 0 or self.html_dir.exists():
                # Count extracted files
                html_files = list(self.html_dir.glob('*.html')) + list(self.html_dir.glob('*.htm'))

                for html_file in html_files:
                    self.topics.append({
                        'path': html_file.name,
                        'file': str(html_file),
                        'size': html_file.stat().st_size
                    })

                print(f"Successfully extracted {len(html_files)} topics")
                return True
            else:
                print(f"hh.exe failed: {result.stderr}", file=sys.stderr)
                return False

        except subprocess.TimeoutExpired:
            print("hh.exe timed out", file=sys.stderr)
            return False
        except Exception as e:
            print(f"Error using hh.exe: {e}", file=sys.stderr)
            return False

    def extract(self) -> bool:
        """Extract CHM content using available method"""
        # Try library first, then fallback to hh.exe
        if HAS_CHM_LIB:
            if self.extract_using_library():
                self.save_metadata()
                return True

        # Fallback to hh.exe
        print("Trying hh.exe fallback...")
        if self.extract_using_hh_exe():
            self.save_metadata()
            return True

        print("All extraction methods failed", file=sys.stderr)
        return False

    def save_metadata(self):
        """Save extraction metadata"""
        metadata = {
            'source_chm': str(self.chm_path),
            'output_dir': str(self.output_dir),
            'topic_count': len(self.topics),
            'topics': self.topics
        }

        metadata_file = self.output_dir / 'extraction_metadata.json'
        metadata_file.write_text(json.dumps(metadata, indent=2), encoding='utf-8')
        print(f"Metadata saved to: {metadata_file}")


def main():
    parser = argparse.ArgumentParser(description='Extract Delphi CHM documentation')
    parser.add_argument('chm_file', help='Path to CHM file')
    parser.add_argument('output_dir', help='Output directory for extracted files')
    parser.add_argument('--verbose', '-v', action='store_true', help='Verbose output')

    args = parser.parse_args()

    try:
        extractor = CHMExtractor(args.chm_file, args.output_dir)
        success = extractor.extract()

        if success:
            print(f"\nExtraction complete!")
            print(f"  Topics: {len(extractor.topics)}")
            print(f"  Output: {extractor.output_dir}")
            sys.exit(0)
        else:
            print("\nExtraction failed!", file=sys.stderr)
            sys.exit(1)

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
