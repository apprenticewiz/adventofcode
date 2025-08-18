#!/usr/bin/env -S gst -f

PackageLoader fileInPackage: 'Digest'.

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <key>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> process: key [
    | md5 n tryKey digest |
    md5 := MD5 new.
    n := 1.
    [
      md5 initialize.
      tryKey := key , (n asString).
      md5 nextPutAll: tryKey.
      digest := md5 hexDigest.
      (digest copyFrom: 1 to: 6) = '000000'
    ] whileFalse: [ n := n + 1 ].
    ^n
  ]

  Program class >> main: args [
    | filename result |
    (args size < 1) ifTrue: [ ^self usage ].
    filename := args first.
    result := self process: filename.
    Transcript show: 'result = '; show: result printString; cr.
  ]
].

Program main: Smalltalk arguments.
