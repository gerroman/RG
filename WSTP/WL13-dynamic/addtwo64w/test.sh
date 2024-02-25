#!/usr/bin/expect

spawn  "./addtwo64w"
expect "Create link: " { send "6464\r" }
interact
