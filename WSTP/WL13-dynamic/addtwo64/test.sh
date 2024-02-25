#!/usr/bin/expect

spawn  "./addtwo64"
expect "Create link: " { send "6464\r" }
interact
