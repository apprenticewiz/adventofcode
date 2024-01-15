#!/usr/bin/bash

progname=$1
shift
args=( $@ )
gst -f ${progname} ${args[@]}
