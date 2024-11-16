#!/usr/bin/expect

spawn  "./addtwo64w.bin"
expect "Create link: " { send "6464\r" }
interact
