# MCP Server for LLM Orchestration

A Model Context Protocol (MCP) server built in Erlang that orchestrates requests to different LLMs (ChatGPT, Gemini, Claude, etc.) using OTP principles.

## Features

- OTP-based architecture for robust, fault-tolerant operation
- Support for multiple LLM providers (OpenAI, Google Gemini, Anthropic Claude)
- Dynamic model registration and management
- RESTful API for chat completions and model listing
- Automatic failover and load balancing between models

## Architecture

The application follows OTP design principles with the following components:

- `mcp_app`: Main application module
- `mcp_sup`: Top-level supervisor
- `mcp_model_sup`: Dynamic supervisor for LLM connections
- `mcp_orchestrator`: Central orchestrator for routing requests
- `mcp_model`: GenServer implementation for each LLM type
- HTTP API handlers for external communication

## Setup

### Prerequisites

- Erlang/OTP 24 or later
- Rebar3

### Building

```bash
cd mcp_server
rebar3 compile
```

### Running

```bash
rebar3 shell
```

## API Endpoints

- `POST /api/v1/chat`: Send a chat request to an LLM
- `GET /api/v1/models`: List all available models

## Configuration

LLM API keys and other configuration should be provided through environment variables or a config file.

## License

MIT
