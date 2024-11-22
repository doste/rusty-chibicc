#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./main "$input" > tmp.s || exit
  
  as -o tmp.o tmp.s
  #gcc -static -o tmp tmp.s
  ld -macos_version_min 14.0.0 -o tmp tmp.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}


assert 0 0
assert 42 42
assert 21 '5+20-4'
assert 41 ' 12 + 34 - 5 '

echo OK
