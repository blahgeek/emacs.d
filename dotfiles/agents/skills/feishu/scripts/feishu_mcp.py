# /// script
# requires-python = ">=3.11"
# dependencies = ["mcp[cli]", "httpx"]
# ///
"""Feishu MCP client - list and call tools via MCP protocol.

Requires environment variables:
    FEISHU_APP_ID      - Feishu app ID
    FEISHU_APP_SECRET  - Feishu app secret

Usage:
    uv run feishu_mcp.py login
    uv run feishu_mcp.py list
    uv run feishu_mcp.py call <tool-name> '<json-arguments>'

Examples:
    uv run feishu_mcp.py login
    uv run feishu_mcp.py list
    uv run feishu_mcp.py call create-doc '{"title": "Test", "markdown": "## Hello"}'
    uv run feishu_mcp.py call fetch-doc '{"doc_id": "docxXXXXXX"}'
"""

import asyncio
import json
import os
import sys
import time
import webbrowser
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from threading import Event
from urllib.parse import parse_qs, urlparse

import httpx
from mcp import ClientSession
from mcp.client.streamable_http import streamablehttp_client

MCP_SERVER_URL = "https://mcp.feishu.cn/mcp"
MCP_ALLOWED_TOOLS = "create-doc,fetch-doc,search-doc,list-docs,update-doc,fetch-file"
OAUTH_TOKEN_URL = "https://open.feishu.cn/open-apis/authen/v2/oauth/token"
AUTHORIZE_URL = "https://accounts.feishu.cn/open-apis/authen/v1/authorize"
AUTH_FILE = Path.home() / ".feishu_mcp.auth.json"
LOGIN_PORT = 19823
OAUTH_SCOPES = " ".join([
    "offline_access",
    # contact
    "contact:user:search",
    "contact:contact.base:readonly",
    "contact:user.base:readonly",
    # docs & drive
    "search:docs:read",
    "wiki:wiki:readonly",
    "wiki:node:read",
    "wiki:node:create",
    "docx:document:create",
    "docx:document:readonly",
    "docx:document:write_only",
    "docs:document.media:upload",
    # "docs:document.media:download",  # not enabled
    "docs:document.comment:read",
    "docs:document.comment:create",
    # board
    "board:whiteboard:node:read",
    "board:whiteboard:node:create",
    # task & im
    # "task:task:read",  # not enabled
    "im:chat:read",
])


# ---------------------------------------------------------------------------
# App credentials
# ---------------------------------------------------------------------------

def _get_app_credentials() -> tuple[str, str]:
    app_id = os.environ.get("FEISHU_APP_ID")
    app_secret = os.environ.get("FEISHU_APP_SECRET")
    if not app_id or not app_secret:
        print("Error: FEISHU_APP_ID and FEISHU_APP_SECRET environment variables are required.", file=sys.stderr)
        sys.exit(1)
    return app_id, app_secret


# ---------------------------------------------------------------------------
# Auth file helpers
# ---------------------------------------------------------------------------

def _load_auth() -> dict | None:
    if not AUTH_FILE.exists():
        return None
    try:
        return json.loads(AUTH_FILE.read_text())
    except (json.JSONDecodeError, OSError):
        return None


def _save_auth(data: dict) -> None:
    AUTH_FILE.write_text(json.dumps(data, indent=2))
    AUTH_FILE.chmod(0o600)


# ---------------------------------------------------------------------------
# OAuth login flow (interactive)
# ---------------------------------------------------------------------------

def _login() -> None:
    """Run interactive OAuth login: open browser, receive callback, exchange code."""
    app_id, app_secret = _get_app_credentials()
    redirect_uri = f"http://localhost:{LOGIN_PORT}/callback"

    code_holder: dict = {}
    done = Event()

    class CallbackHandler(BaseHTTPRequestHandler):
        def do_GET(self):
            qs = parse_qs(urlparse(self.path).query)
            if "code" in qs:
                code_holder["code"] = qs["code"][0]
                self.send_response(200)
                self.send_header("Content-Type", "text/html; charset=utf-8")
                self.end_headers()
                self.wfile.write(b"<h2>Authorization successful! You can close this tab.</h2>")
            else:
                self.send_response(400)
                self.send_header("Content-Type", "text/html; charset=utf-8")
                self.end_headers()
                err = qs.get("error", ["unknown"])[0]
                self.wfile.write(f"<h2>Authorization failed: {err}</h2>".encode())
            done.set()

        def log_message(self, format, *args):
            pass  # suppress request logs

    server = HTTPServer(("127.0.0.1", LOGIN_PORT), CallbackHandler)

    auth_url = f"{AUTHORIZE_URL}?client_id={app_id}&redirect_uri={redirect_uri}&response_type=code&scope={OAUTH_SCOPES}"
    print(f"Opening browser for Feishu authorization...")
    print(f"If the browser does not open, visit:\n  {auth_url}\n")
    webbrowser.open(auth_url)

    print("Waiting for authorization callback...")
    while not done.is_set():
        server.handle_request()
    server.server_close()

    if "code" not in code_holder:
        print("Error: did not receive authorization code.", file=sys.stderr)
        sys.exit(1)

    # Exchange code for tokens
    resp = httpx.post(OAUTH_TOKEN_URL, json={
        "grant_type": "authorization_code",
        "client_id": app_id,
        "client_secret": app_secret,
        "code": code_holder["code"],
        "redirect_uri": redirect_uri,
    }, timeout=10)
    data = resp.json()
    if data.get("code") != 0:
        print(f"Error: token exchange failed: {data}", file=sys.stderr)
        sys.exit(1)

    auth = {
        "access_token": data["access_token"],
        "access_token_expires_at": time.time() + data["expires_in"],
        "refresh_token": data.get("refresh_token", ""),
        "refresh_token_expires_at": time.time() + data.get("refresh_token_expires_in", 0),
    }
    _save_auth(auth)
    print("Login successful! Tokens saved to ~/.feishu_mcp.auth.json")


# ---------------------------------------------------------------------------
# Token management
# ---------------------------------------------------------------------------

def _refresh_access_token(auth: dict) -> dict:
    """Use refresh_token to get a new access_token."""
    app_id, app_secret = _get_app_credentials()
    resp = httpx.post(OAUTH_TOKEN_URL, json={
        "grant_type": "refresh_token",
        "client_id": app_id,
        "client_secret": app_secret,
        "refresh_token": auth["refresh_token"],
    }, timeout=10)
    data = resp.json()
    if data.get("code") != 0:
        print(f"Error: token refresh failed (code={data.get('code')}): {data.get('msg', data)}", file=sys.stderr)
        print("Please run `feishu_mcp.py login` to re-authorize.", file=sys.stderr)
        sys.exit(1)

    auth = {
        "access_token": data["access_token"],
        "access_token_expires_at": time.time() + data["expires_in"],
        "refresh_token": data.get("refresh_token", auth["refresh_token"]),
        "refresh_token_expires_at": time.time() + data.get("refresh_token_expires_in", 0),
    }
    _save_auth(auth)
    return auth


def _get_user_access_token() -> str:
    """Return a valid user_access_token, refreshing if needed."""
    auth = _load_auth()
    if not auth:
        print("Error: not logged in. Run `feishu_mcp.py login` first.", file=sys.stderr)
        sys.exit(1)

    # Check if refresh_token itself is expired
    if auth.get("refresh_token_expires_at", 0) < time.time():
        print("Error: refresh token expired. Run `feishu_mcp.py login` to re-authorize.", file=sys.stderr)
        sys.exit(1)

    # Refresh access_token if it expires within 5 minutes
    if auth.get("access_token_expires_at", 0) < time.time() + 300:
        auth = _refresh_access_token(auth)

    return auth["access_token"]


# ---------------------------------------------------------------------------
# MCP headers & operations
# ---------------------------------------------------------------------------

def _build_headers() -> dict:
    token = _get_user_access_token()
    return {
        "X-Lark-MCP-UAT": token,
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

    if command == "login":
        _login()

    elif command == "list":
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
        print("Available commands: login, list, call")
        sys.exit(1)


if __name__ == "__main__":
    main()
