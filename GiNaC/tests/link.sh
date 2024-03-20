#!/bin/env expect

spawn  "./bin/G.exe"
expect "Create link: " { send "6464\r" }
interact
