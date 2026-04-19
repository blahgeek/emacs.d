#!/usr/bin/env bash
# Thin wrapper around the Tavily HTTP API.
#
# Usage: tavily.sh <endpoint> <json-body>
#
# <endpoint> is one of: search | extract | crawl | map
# <json-body> is the raw JSON body string passed as-is to curl.
#
# Requires the TAVILY_API_KEY environment variable.

set -euo pipefail

if [[ $# -ne 2 ]]; then
    echo "Usage: $0 <endpoint> <json-body>" >&2
    exit 2
fi

if [[ -z "${TAVILY_API_KEY:-}" ]]; then
    echo "TAVILY_API_KEY is not set" >&2
    exit 2
fi

endpoint=$1
body=$2

exec curl -sS -X POST "https://api.tavily.com/${endpoint}" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer ${TAVILY_API_KEY}" \
    -d "${body}"
