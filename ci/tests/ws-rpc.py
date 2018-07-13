#!/usr/bin/env python3
import asyncio
import websockets
from jsonrpcclient.websockets_client import WebSocketsClient


async def main():
    async with websockets.connect('ws://localhost:3145/wallet') as ws:
        response = await WebSocketsClient(ws).request('ping')
        print(response)

asyncio.get_event_loop().run_until_complete(main())
