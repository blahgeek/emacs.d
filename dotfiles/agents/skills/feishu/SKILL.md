---
name: feishu
description: Access Feishu (Lark) document tools via MCP protocol. Provides 5 tools for creating, reading, listing, updating docs, and fetching files. Use when the user wants to interact with Feishu cloud documents.
---

# Feishu MCP Skill

通过 MCP 协议访问飞书云文档，提供文档的创建、读取、列表、更新和文件获取功能。

## Available Tools

| Tool | Description |
|------|-------------|
| `create-doc` | 从 Markdown 创建飞书云文档，支持异步模式 |
| `fetch-doc` | 获取文档内容，返回 Markdown 格式，支持分页 |
| `list-docs` | 获取知识节点下的子文档列表 |
| `update-doc` | 更新文档内容，支持 7 种模式（覆盖/追加/替换/插入/删除） |
| `fetch-file` | 通过 token 获取文件/图片/画板的 Base64 内容 |

## Prerequisites

需要设置环境变量：

- `FEISHU_APP_ID` - 飞书应用 App ID
- `FEISHU_APP_SECRET` - 飞书应用 App Secret

脚本会自动通过这两个凭证获取 `tenant_access_token`。

## How to Call Tools

使用 `scripts/feishu_mcp.py` 通过 MCP 协议调用工具（在 skill 根目录下运行）：

```bash
# 列出所有工具
uv run --with "mcp[cli],httpx" python scripts/feishu_mcp.py list

# 调用工具
uv run --with "mcp[cli],httpx" python scripts/feishu_mcp.py call <tool-name> '<json-arguments>'
```

也可在 Python 中直接导入使用：

```python
import asyncio
import sys
sys.path.insert(0, "<skill-root>/scripts")
from feishu_mcp import list_tools, call_tool

# 列出工具
tools = asyncio.run(list_tools())

# 调用工具
result = asyncio.run(call_tool("fetch-doc", {"doc_id": "docxXXXXXX"}))
```

## Tool Details

### create-doc

从 Markdown 内容创建飞书云文档。支持写入知识库节点、知识空间或文件夹。

**Parameters:**
- `title` (string) - 文档标题
- `markdown` (string) - Markdown 内容
- `wiki_node` (string, optional) - 知识库节点 token（与 wiki_space/folder_token 互斥）
- `wiki_space` (string, optional) - 知识空间 ID（与 wiki_node/folder_token 互斥）
- `folder_token` (string, optional) - 父文件夹 token（与 wiki_node/wiki_space 互斥）
- `task_id` (string, optional) - 查询异步任务状态

**Example:**
```bash
uv run --with "mcp[cli],httpx" python scripts/feishu_mcp.py call create-doc '{"title": "项目计划", "markdown": "## 目标\n\n- 目标 1\n- 目标 2"}'
```

### fetch-doc

获取飞书云文档内容，返回 Markdown 格式。

**Parameters:**
- `doc_id` (string, required) - 文档 ID 或 URL
- `offset` (integer, optional) - 字符偏移量，用于分页
- `limit` (integer, optional) - 最大返回字符数

**Example:**
```bash
uv run --with "mcp[cli],httpx" python scripts/feishu_mcp.py call fetch-doc '{"doc_id": "docxXXXXXX"}'
```

### list-docs

获取知识节点下的子文档列表。

**Parameters:**
- `doc_id` (string) - Wiki 文档 URL（my_library=false 时必填）
- `my_library` (boolean) - true 查询个人文档库
- `page_size` (integer) - 每页数量 1-50，默认 10
- `page_token` (string) - 分页标记

**Example:**
```bash
uv run --with "mcp[cli],httpx" python scripts/feishu_mcp.py call list-docs '{"my_library": true, "page_size": 10}'
```

### update-doc

更新飞书云文档，支持 7 种模式。

**Parameters:**
- `doc_id` (string) - 文档 ID 或 URL
- `mode` (string, required) - overwrite / append / replace_range / replace_all / insert_before / insert_after / delete_range
- `markdown` (string) - Markdown 内容
- `new_title` (string, optional) - 新标题
- `selection_by_title` (string, optional) - 标题定位 `## 章节标题`
- `selection_with_ellipsis` (string, optional) - 内容定位 `开头...结尾`
- `task_id` (string, optional) - 查询异步任务状态

**Example:**
```bash
# 追加内容
uv run --with "mcp[cli],httpx" python scripts/feishu_mcp.py call update-doc '{"doc_id": "docxXXX", "mode": "append", "markdown": "## 新章节\n\n追加内容"}'

# 替换章节
uv run --with "mcp[cli],httpx" python scripts/feishu_mcp.py call update-doc '{"doc_id": "docxXXX", "mode": "replace_range", "selection_by_title": "## 旧章节", "markdown": "## 新章节\n\n更新内容"}'
```

### fetch-file

获取文件/图片/画板的 Base64 内容。

**Parameters:**
- `resource_token` (string, required) - file_token / image_token / whiteboard_token
- `type` (string) - media（默认）或 whiteboard

**Examples:**
```bash
# 获取文件或图片，输出 Base64 到文件
uv run --with "mcp[cli],httpx" python scripts/feishu_mcp.py call fetch-file '{"resource_token": "boxcnXXXX"}' \
  | jq -r '.[0].data' | base64 -d > image.jpg

# 获取白板图（文档中的 <whiteboard token="xxx"/> 标签）
# 先通过 fetch-doc 获取文档，找到白板 token，再用 type=whiteboard 获取
uv run --with "mcp[cli],httpx" python scripts/feishu_mcp.py call fetch-file '{"resource_token": "Carfw7XPEhbpozbBV01cUpOznub", "type": "whiteboard"}' \
  | jq -r '.[0].data' | base64 -d > whiteboard.jpg
```

## Notes

- 文档创建支持飞书扩展 Markdown 语法（分栏、高亮块、Mermaid 等）
- 大文档操作可能返回 `task_id`，需再次调用同一工具传入 `task_id` 查询状态
- `update-doc` 优先使用局部更新（append/replace_range），慎用 overwrite
