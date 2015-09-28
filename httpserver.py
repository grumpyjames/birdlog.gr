#! /usr/bin/env python2
from SimpleHTTPServer import SimpleHTTPRequestHandler
import BaseHTTPServer
import time

class PostRequestHandler (SimpleHTTPRequestHandler):
    def do_POST(self):
        time.sleep(3)
        self.send_response(201)
        self.send_header("Content-type", "application/json")
        self.send_header("Content-Length", "2")
        self.end_headers()
        self.wfile.write("{}")


if __name__ == '__main__':
    BaseHTTPServer.test(PostRequestHandler, BaseHTTPServer.HTTPServer)
