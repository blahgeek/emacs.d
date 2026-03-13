---
name: aider
description: Process AI! markers in code to implement features or fix bugs. Use ONLY when the user explicitly asks to run the aider skill. Scans for AI! comments, generates code changes, and removes the markers.
---

# Aider Skill

AI-assisted coding workflow that processes code marked with `AI!` comments.

## Workflow

1. **Find Markers** - Scan codebase for `AI!` markers inside comments only
2. **Analyze Context** - Read surrounding code to understand intent
3. **Generate Code** - Implement, fix, or add features
4. **Cleanup** - Remove the AI! marker comment after implementation

## When to Run

**Only execute this skill when the user explicitly asks you to run the aider skill.** Do not automatically scan for AI! markers unless the user specifically requests it.

## Usage

### Step 1: Find AI! Markers

Use `rg` (ripgrep) to find all "AI!" markers.

**Always use absolute path** (required):
```bash
rg "AI!" -B 5 -A 10 <absolute-path>
```

Examples:
- `rg "AI!" -B 5 -A 10 /home/user/project` - Search current project
- `rg "AI!" -B 5 -A 10 /home/user/project/src` - Search specific directory
- `rg "AI!" -B 5 -A 10 /path/to/project` - Search specific project

Never use relative paths like `.` or `./src` - always use the full absolute path.

This shows context around each marker so you can understand what needs to be implemented.

### Step 2: Filter Valid Markers

After finding all occurrences with ripgrep, **verify each marker is inside a comment**:

- Line starts with comment syntax: `#`, `//`, `/*`, `*`, `<!--`, `--`, `;`, etc.
- The `AI!` is part of the comment text, not in string literals, variable names, or data

**Running `rg` once is sufficient. Do not further refine the command or using other tools like grep.**
**If no valid markers are found after filtering, do nothing and exit.**

#### Valid vs Invalid Markers

**Valid markers** (inside comments):
```python
# AI! Implement error handling for edge cases
def process():
    pass
```

```javascript
// AI! Add input validation here
function validate() {
    return true;
}
```

```rust
/* AI! Optimize this for performance */
fn compute() {
    // ...
}
```

**Invalid markers** (ignore these):
```python
message = "AI! is the future"  # String content, not a comment instruction
print("AI! marker detected")   # String literal
```

```javascript
const label = "AI! Processing";  // Part of a string value
console.log("AI! completed");    // String literal
```

```python
data = {"AI!": True}  # Dictionary key, not a comment
```

### Step 3: Analyze Each Valid Marker

For each valid marker found:

1. **Read the comment** containing `AI!` - this describes the task
2. **Read the surrounding context**:
   - Before: Function signature, class definition, imports, docstrings
   - After: Existing code structure, related functions, patterns to follow
3. **Understand the intent**: Is it a new function to implement? A bug to fix? A feature to add?

### Step 4: Generate Code

Based on the marker context:

- **If implementing a new function**: Write the complete function body
- **If fixing a bug**: Identify and correct the problematic code
- **If adding a feature**: Insert the new code at appropriate location

**Code Style Guidelines**:
- Match the existing code style (indentation, naming conventions)
- Follow language-specific best practices
- Add appropriate error handling if needed
- Include docstrings/comments if the project uses them

### Step 5: Cleanup

**After implementing the code, remove the AI! marker comment line.**

The marker is temporary documentation - once the code is implemented, it's no longer needed.

Example:
```python
# Before:
# AI! Implement a function to calculate factorial recursively
def factorial(n):
    pass

# After:
def factorial(n):
    """Calculate factorial recursively."""
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

## Marker Format Examples

### Implementing a function:
```python
# AI! Implement a function to calculate factorial recursively
def factorial(n):
    pass
```

### Fixing a bug:
```python
# Fix: handle empty list case to avoid IndexError. AI!
def get_first(items):
    return items[0]  # Bug: no check for empty list
```

### Adding a feature:
```javascript
// AI! Add input validation for email format
function processUser(user) {
    // existing code
}
```

### Refactoring:
```rust
// Refactor: extract this into a separate helper function
// AI!
fn process_data(data: &str) -> Result<String, Error> {
    // long implementation...
}
```

### Multi-line comment:
```python
# AI! Implement caching for this expensive operation
# The cache should use LRU eviction policy with max size of 100
# and expire entries after 5 minutes
def fetch_data(key):
    pass
```
