#!/usr/bin/expect

spawn  "./addtwo64.bin"
expect "Create link: " { send "6464\r" }
interact
