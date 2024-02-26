#!/usr/bin/expect

spawn  "./G.bin"
expect "Create link: " { send "6464\r" }
interact
