#!/usr/bin/env dart

import 'dart:convert';
import 'dart:io';
import 'package:crypto/crypto.dart';

void usage(String progName) {
    print('usage: $progName <key>');
    exit(1);
}

int process(String key) {
    int n = 1;
    while ( true ) {
        String tryKey = key + n.toString();
        final bytes = utf8.encode(tryKey);
        final digest = md5.convert(bytes);
        String hexDigest = digest.toString();
        if ( hexDigest.startsWith("00000") ) {
            return n;
        }
        n++;
    }
}

void main(List<String> args) {
    if ( args.isEmpty ) {
        usage(Platform.script.pathSegments.last);
    }
    final key = args[0];
    final result = process(key);
    print('result = $result');
}
