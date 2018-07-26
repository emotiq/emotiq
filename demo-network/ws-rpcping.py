#!/usr/bin/env python3
import asyncio
import websockets
import sys
from jsonrpc_websocket import Server

if len(sys.argv) > 1:
    node_offset = int(sys.argv[1])
else:
    node_offset = 1


async def main():
    server = Server("ws://localhost:%d/wallet" % (4144+node_offset))
    try:
        await server.ws_connect()
        resp = await server.ping()
        print(resp)
    except Exception as e:
        print("Failed to ping Node %d: %s" % (node_offset, str(e)))
        sys.exit(1)
    finally:
        await server.close()
        await server.session.close()

asyncio.get_event_loop().run_until_complete(main())
