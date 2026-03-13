"""Feishu MCP client - list and call tools via MCP protocol.

Requires environment variables:
    FEISHU_APP_ID      - Feishu app ID
    FEISHU_APP_SECRET  - Feishu app secret

Usage:
    uv run --with "mcp[cli],httpx" python feishu_mcp.py list
    uv run --with "mcp[cli],httpx" python feishu_mcp.py call <tool-name> '<json-arguments>'

Examples:
    uv run --with "mcp[cli],httpx" python feishu_mcp.py list
    uv run --with "mcp[cli],httpx" python feishu_mcp.py call create-doc '{"title": "Test", "markdown": "## Hello"}'
    uv run --with "mcp[cli],httpx" python feishu_mcp.py call fetch-doc '{"doc_id": "docxXXXXXX"}'
"""

import asyncio
import json
import os
import sys

import httpx
from mcp import ClientSession
from mcp.client.streamable_http import streamablehttp_client

MCP_SERVER_URL = "https://mcp.feishu.cn/mcp"
MCP_ALLOWED_TOOLS = "create-doc,fetch-doc,search-doc,list-docs,update-doc,fetch-file"
TOKEN_URL = "https://open.feishu.cn/open-apis/auth/v3/tenant_access_token/internal"


def _get_tenant_access_token() -> str:
    """Fetch tenant_access_token from Feishu API using app credentials."""
    app_id = os.environ.get("FEISHU_APP_ID")
    app_secret = os.environ.get("FEISHU_APP_SECRET")
    if not app_id or not app_secret:
        print("Error: FEISHU_APP_ID and FEISHU_APP_SECRET environment variables are required.", file=sys.stderr)
        sys.exit(1)

    resp = httpx.post(TOKEN_URL, json={"app_id": app_id, "app_secret": app_secret}, timeout=10)
    data = resp.json()
    if data.get("code") != 0:
        print(f"Error: Failed to get tenant_access_token: {data.get('msg')}", file=sys.stderr)
        sys.exit(1)
    return data["tenant_access_token"]


def _build_headers() -> dict:
    token = _get_tenant_access_token()
    return {
        "X-Lark-MCP-TAT": token,
        "X-Lark-MCP-Allowed-Tools": MCP_ALLOWED_TOOLS,
    }


async def list_tools() -> list[dict]:
    """List all available tools from the Feishu MCP server.

    Returns a list of dicts with keys: name, description, inputSchema.
    """
    headers = _build_headers()

    async with streamablehttp_client(MCP_SERVER_URL, headers=headers) as (read, write, _):
        async with ClientSession(read, write) as session:
            await session.initialize()
            result = await session.list_tools()
            return [
                {
                    "name": tool.name,
                    "description": tool.description,
                    "inputSchema": tool.inputSchema,
                }
                for tool in result.tools
            ]


async def call_tool(tool_name: str, arguments: dict | None = None) -> list[dict]:
    """Call a tool on the Feishu MCP server.

    Returns a list of content blocks from the tool response.
    """
    headers = _build_headers()

    async with streamablehttp_client(MCP_SERVER_URL, headers=headers) as (read, write, _):
        async with ClientSession(read, write) as session:
            await session.initialize()
            result = await session.call_tool(tool_name, arguments=arguments or {})
            blocks = []
            for block in result.content:
                entry = {"type": block.type}
                if block.type == "text":
                    entry["text"] = block.text
                elif block.type == "image":
                    entry["data"] = block.data
                    entry["mimeType"] = getattr(block, "mimeType", "image/jpeg")
                else:
                    entry["text"] = str(block)
                blocks.append(entry)
            return blocks


def _print_tools(tools: list[dict]) -> None:
    for tool in tools:
        print(f"## {tool['name']}")
        print(f"Description: {tool['description']}")
        if tool.get("inputSchema"):
            print(f"Parameters: {json.dumps(tool['inputSchema'], indent=2, ensure_ascii=False)}")
        print()


def _print_result(content_blocks: list[dict]) -> None:
    # Single text block: print text directly for readability
    if len(content_blocks) == 1 and content_blocks[0]["type"] == "text":
        print(content_blocks[0]["text"])
    else:
        # Multiple blocks or non-text: output JSON for piping to jq
        print(json.dumps(content_blocks, ensure_ascii=False))


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    command = sys.argv[1]

    if command == "list":
        tools = asyncio.run(list_tools())
        print(f"Found {len(tools)} tools:\n")
        _print_tools(tools)

    elif command == "call":
        if len(sys.argv) < 3:
            print("Usage: feishu_mcp.py call <tool-name> [json-arguments]")
            sys.exit(1)
        tool_name = sys.argv[2]
        arguments = json.loads(sys.argv[3]) if len(sys.argv) > 3 else {}
        result = asyncio.run(call_tool(tool_name, arguments))
        _print_result(result)

    else:
        print(f"Unknown command: {command}")
        print("Available commands: list, call")
        sys.exit(1)


if __name__ == "__main__":
    main()
