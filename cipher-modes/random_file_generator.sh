#!/bin/bash
head -c 10KB </dev/urandom > random_file_10KB.txt
head -c 10MB </dev/urandom > random_file_10MB.txt
head -c 100MB </dev/urandom > random_file_100MB.txt
