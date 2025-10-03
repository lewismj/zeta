from __future__ import annotations

"""
Simple TCP REPL server for Zeta Lisp.

Protocol: JSON per line over TCP.
- Request: {"cmd": "eval", "code": "(progn ...)"}
- Response: {"ok": true, "result": <repr string>} or {"ok": false, "error": <message>}

This server is stateless per request other than keeping an Interpreter alive
so that definitions persist across evaluations.
"""

import json
import socket
import threading
from typing import Tuple

from zeta.interpreter import Interpreter


HOST = "127.0.0.1"
PORT = 8765


class ReplServer:
    def __init__(self, host: str = HOST, port: int = PORT):
        self.host = host
        self.port = port
        # Keep a single interpreter to maintain session state
        self.interp = Interpreter(prelude=None)

    def serve_forever(self):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            s.bind((self.host, self.port))
            s.listen(5)
            while True:
                conn, addr = s.accept()
                threading.Thread(target=self._handle_client, args=(conn, addr), daemon=True).start()

    def _handle_client(self, conn: socket.socket, addr: Tuple[str, int]):
        with conn:
            buf = b""
            while True:
                data = conn.recv(4096)
                if not data:
                    break
                buf += data
                while b"\n" in buf:
                    line, buf = buf.split(b"\n", 1)
                    line = line.strip()
                    if not line:
                        continue
                    try:
                        req = json.loads(line.decode("utf-8"))
                        if req.get("cmd") == "eval":
                            code = req.get("code", "")
                            try:
                                result = self.interp.eval(code)
                                # Convert result to a printable string; Zeta values are Python values/objects
                                resp = {"ok": True, "result": repr(result)}
                            except Exception as ex:
                                resp = {"ok": False, "error": str(ex)}
                        else:
                            resp = {"ok": False, "error": f"Unknown cmd: {req.get('cmd')}"}
                    except Exception as ex:
                        resp = {"ok": False, "error": f"Invalid request: {ex}"}
                    conn.sendall((json.dumps(resp) + "\n").encode("utf-8"))


if __name__ == "__main__":
    ReplServer().serve_forever()
