#!/bin/sh
cargo run -- --emit asm samples/v1_ultrakill.l > program.nasm && nasm -felf64 -o program.o program.nasm && ld program.o out_int.o
