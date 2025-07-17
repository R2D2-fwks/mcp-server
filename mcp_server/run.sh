#!/bin/sh

# Build and run script for MCP Server

# Ensure we're in the correct directory
cd "$(dirname "$0")"

echo "=== Building MCP Server ==="
if command -v rebar3 > /dev/null 2>&1; then
    rebar3 compile
else
    echo "Error: rebar3 not found. Please install rebar3 to build this project."
    exit 1
fi

echo "=== Starting MCP Server ==="
rebar3 shell
