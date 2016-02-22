#!/bin/sh
( echo "{-# LINE 1 \"$2\" #-}" ; iconv -f utf-8 -t l1 $2 ) > $3
