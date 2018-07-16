#!/usr/bin/env python3

import asyncio
import websockets
import json
import sys


# ping_data = {"jsonrpc": "2.0", "method": "ping", "id": 1}
ping_data = {"jsonrpc": "2.0", "method": "ping1", "id": 1}

if len(sys.argv) > 1:
    node_offset = int(sys.argv[1])
else:
    node_offset = 1


async def hello():
    try:
        async with websockets.connect(
                "ws://localhost:%d/wallet" % (3144+node_offset)) as websocket:
            await websocket.send(json.dumps(ping_data))
            print(f"> {ping_data}")

            resp = await websocket.recv()
            print(f"< {resp}")
    except:
        print("Failed to ping!")
        sys.exit(1)

asyncio.get_event_loop().run_until_complete(hello())
