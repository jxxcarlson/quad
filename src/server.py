    
"""
server.py responds to the requests

   http://localhost:8000/ledOn
   http://localhost:8000/ledOff
   http://localhost:8000/distance
   http://localhost:8000/cleanup
   http://localhost:8000/params
   http://localhost:8000/shutdown

where params are for the Quad frontend.

Run the server on the Raspberry Pi with `sudo ./server.py 8000`
"""



"""
Source: https://gist.github.com/bradmontgomery/2219997

Very simple HTTP server in python.
Usage::
    ./server.py 8000
Send a GET request::
    curl http://localhost:8000/params
Send a HEAD request::
    curl -I http://localhost:8000
Send a POST request::
    curl -d "foo=bar&bin=baz" http://localhost:8000
"""

import RPi.GPIO as GPIO
import time
# from time import sleep

GPIO.setmode(GPIO.BCM)
print GPIO.getmode()

led = 17
GPIO.setup(led, GPIO.OUT)

led2 = 22
GPIO.setup(led2, GPIO.OUT)

def ledOn():
  GPIO.output(led, 1)

def ledOff():
  GPIO.output(led, 0)

def led2On():
  GPIO.output(led2, 1)

def led2Off():
  GPIO.output(led2, 0)

## Ultrasonic distance measurement:

#set GPIO Pins
GPIO_TRIGGER = 4
GPIO_ECHO = 27

#set GPIO direction (IN / OUT)
GPIO.setup(GPIO_TRIGGER, GPIO.OUT)
GPIO.setup(GPIO_ECHO, GPIO.IN)

def distance():
    # set Trigger to HIGH
    GPIO.output(GPIO_TRIGGER, True)

    # set Trigger after 0.01ms to LOW
    time.sleep(0.00001)
    GPIO.output(GPIO_TRIGGER, False)

    StartTime = time.time()
    StopTime = time.time()

    # save StartTime
    while GPIO.input(GPIO_ECHO) == 0:
        StartTime = time.time()

    # save time of arrival
    while GPIO.input(GPIO_ECHO) == 1:
        StopTime = time.time()

    # time difference between start and arrival
    TimeElapsed = StopTime - StartTime
    # multiply with the sonic speed (34300 cm/s)
    # and divide by 2, because there and back
    distance = (TimeElapsed * 34300) / 2

    return distance


from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import SocketServer
import os
import random
import json

def colorRange(a, b, c, d, e, f, g, h):
    return [{'lo' : a, 'hi' : b}, {'lo' : c, 'hi' : d}, {'lo': e, 'hi' : f}, {'lo': g, 'hi': h}]

def proportions(a, b, c, d):
    return [a, b, c, d]

def parameters(a, b, c, d):
    return json.dumps({'colorRange': colorRange(a, b, c, d, 0.0, 1.0, 0.99, 1.0), 'proportions' : [ 0.4, 0.5, 0.3, 0.7 ], 'maxDepth' : 5})


# Generate a series of n numbers
# by adding random -1, +1 to an
# initial value
def generate(initial_value):
    x = initial_value
    n = 0
    output = [x]
    while x > 0 and n < 5000:
        x = x + 2*random.randint(0,1) - 1
        output.append(x)
        n = n + 1
    return output


running = True

class S(BaseHTTPRequestHandler):


    def _set_headers(self):
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()

    def do_GET(self):
        if self.path.find("/params") == 0:
          data = parameters(0.5, 0.6, 0.2, 0.4 )
          message = data # data
        elif self.path.find("/distance") == 0:
          led2On()
          message = distance()
        elif self.path.find("/ledOn") == 0:
          message = "led on"
          ledOn()
          led2Off()
        elif self.path.find("/ledOff") == 0:
          message = "led off"
          ledOff()
        elif self.path.find("/led2On") == 0:
          message = "led2 on"
          led2On()
        elif self.path.find("/led2Off") == 0:
          message = "led2 off"
          led2Off()
        elif self.path.find("/cleanup") == 0:
          message = "cleanup"
          GPIO.cleanup()
        elif self.path.find("/shutdown") == 0:
          message = "Shutting down"
          GPIO.cleanup()
          running = False
        else:
          message = "I don't undersand."
        self._set_headers()
        self.wfile.write(message)

    def do_HEAD(self):
        self._set_headers()

    def do_POST(self):
        # Doesn't do anything with posted data
        self._set_headers()
        self.wfile.write("<html><body><h1>POST!</h1></body></html>")

def keep_running():
  if running:
    return True
  else:
    print "False"
    return False

def run(server_class=HTTPServer, handler_class=S, port=8000):
    server_address = ('', port)
    httpd = server_class(server_address, handler_class)
    print 'Starting httpd...'
    httpd.serve_forever()

def run_while_true(server_class=HTTPServer,
                   handler_class=S, port=8000):
    """
    This assumes that keep_running() is a function of no arguments which
    is tested initially and after each request.  If its return value
    is true, the server continues.
    """
    server_address = ('', 8000)
    httpd = server_class(server_address, handler_class)
    print "Starting httpd ..."
    while keep_running():
        httpd.handle_request()

if __name__ == "__main__":
    from sys import argv

    if len(argv) == 2:
        run_while_true(port=int(argv[1]))
    else:
        run_while_true()
