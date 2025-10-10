""" python gl_maker.py gl.txt """

import os
from pathlib import Path


class TreeParser:
    """Parses ASCII tree format and creates directory structure."""
    
    def __init__(self):
        # Tree drawing characters used in ASCII trees
        self.tree_chars = ['├', '└', '│', '─', '┌', '┐', '┘', '┴', '┬', '┤', '┼', '▶', '▼']
        
    def parse_tree_file(self, file_path, base_path=None):
        """Parse a tree file and create the structure."""
        if base_path is None:
            base_path = Path.cwd()
        else:
            base_path = Path(base_path)
            
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.readlines()
            
        self.create_structure_from_lines(lines, base_path)
        
    def parse_tree_string(self, tree_string, base_path=None):
        """Parse a tree string and create the structure."""
        if base_path is None:
            base_path = Path.cwd()
        else:
            base_path = Path(base_path)
            
        lines = tree_string.strip().split('\n')
        self.create_structure_from_lines(lines, base_path)
        
    def create_structure_from_lines(self, lines, base_path):
        """Create directory structure from parsed lines."""
        path_stack = []
        self._processed_root = False  # Reset for each parse
        root_skipped = False
        
        for line in lines:
            if not line.strip(): continue

            if not root_skipped:  # skip first line
                root_skipped = True
                continue

            depth, name = self._parse_line(line) # extract name & file depth
            
            if depth is None or not name: continue # check any error
                
            # Adjust path stack to current depth
            while len(path_stack) > depth: path_stack.pop()
                
            # Build current path
            if depth == 0: # root depth
                current_path = base_path / name
                path_stack = [current_path]
            else:
                # Child level
                parent_path = path_stack[-1] if path_stack else base_path
                current_path = parent_path / name
                path_stack.append(current_path)
                
            # Create directory or file
            self._create_item(current_path, name)
            print(f"Created: {current_path}")
            
    def _parse_line(self, line):
        """Parse a single line to extract depth and name."""
        if not line.strip(): return None, None
            
        line = line.rstrip() # rear strip (whitespaces at the end of the string only)
        
        if not any(char in line for char in '├└│─'): return 0, line.strip() # root

        depth = 0 # count depth by counting characters before file name
        for char in line:
            if char in '├└─│ ': depth += 0.25
            else: break
                
        # Find the name after the tree characters
        import re
        name_match = re.search(r'[├└][─\s]*(.+?)$', line)
        if name_match: name = name_match.group(1).strip()
        else: # take everything after the last tree character
            last_tree_pos = max(line.rfind('├'), line.rfind('└'), line.rfind('│'))
            if last_tree_pos >= 0: name = line[last_tree_pos + 1:].lstrip('─ ').strip() # remove lead chars
            else: name = line.strip()
                
        return int(depth-1), name
        
    def _create_item(self, path, name):
        """Create a file or directory based on the name."""
        try:
            if self._is_file(name):
                # Create parent directories if they don't exist
                path.parent.mkdir(parents=True, exist_ok=True)
                # Create empty file
                path.touch()
            else:
                # Create directory
                path.mkdir(parents=True, exist_ok=True)
        except Exception as e:
            print(f"Error creating {path}: {e}")
            
    def _is_file(self, name):
        """Determine if an item should be a file or directory."""
        # Root folder should be a directory
        if not hasattr(self, '_processed_root'):
            self._processed_root = True
            return False
            
        # Check if it has a file extension (but not hidden files like .gitignore)
        if '.' in name:
            # Files like .gitignore, .env etc. are files
            if name.startswith('.'):
                return True
            # Files with extensions are files
            parts = name.split('.')
            if len(parts) > 1 and parts[-1]:  # Has a non-empty extension
                return True
        
        # Special cases for common files without extensions
        common_files = {'README', 'LICENSE', 'CHANGELOG', 'Makefile', 'Dockerfile', 'MANIFEST'}
        if name in common_files:
            return True
            
        # Default to directory
        return False


def main():
    """Main function to run the tree builder."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Build directory structure from ASCII tree format')
    parser.add_argument('input_file', help='Input file containing ASCII tree')
    parser.add_argument('-o', '--output', help='Output base directory (default: current directory)')
    parser.add_argument('-d', '--dry-run', action='store_true', help='Show what would be created without actually creating')
    
    args = parser.parse_args()
    
    if not os.path.exists(args.input_file):
        print(f"Error: Input file '{args.input_file}' not found")
        return 1
        
    output_dir = args.output if args.output else os.getcwd()
    
    print(f"Reading tree from: {args.input_file}")
    print(f"Creating structure in: {output_dir}")
    print("-" * 50)
    
    if args.dry_run:
        print("DRY RUN MODE - Nothing will be created")
        print("-" * 50)
        
    tree_parser = TreeParser()
    
    if not args.dry_run:
        tree_parser.parse_tree_file(args.input_file, output_dir)
    else:
        # For dry run, just show what would be created
        with open(args.input_file, 'r') as f:
            lines = f.readlines()
        
        # Skip the root line for dry run too
        root_skipped = False
        for line in lines:
            if line.strip():
                if not root_skipped:
                    root_skipped = True
                    continue
                depth, name = tree_parser._parse_line(line)
                if name:
                    indent = "  " * depth if depth else ""
                    file_type = "FILE" if tree_parser._is_file(name) else "DIR"
                    print(f"{indent}{name} ({file_type})")
    
    print("-" * 50)
    print("Structure creation completed!")
    return 0


def create_structure_from_file(tree_file_path, output_base_path=None):
    """Convenience function to create structure from a tree file."""
    parser = TreeParser()
    parser.parse_tree_file(tree_file_path, output_base_path)


def create_structure_from_string(tree_string, output_base_path=None):
    """Convenience function to create structure from a tree string."""
    parser = TreeParser()
    parser.parse_tree_string(tree_string, output_base_path)


if __name__ == "__main__":
    import sys
    sys.exit(main())
